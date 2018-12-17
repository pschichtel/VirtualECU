package tel.schich.virtualecu

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ThreadFactory

import com.twitter.util.Eval
import net.jcazevedo.moultingyaml._
import tel.schich.javacan.NativeRawCanSocket
import tel.schich.javacan.isotp.AggregatingFrameHandler.aggregateFrames
import tel.schich.javacan.isotp._
import tel.schich.obd4s.ObdBridge
import tel.schich.obd4s.ObdHelper.{asHex, hexDump}

import scala.io.Source

object Main {

    val Name = "Virtual ECU"

    def main(args: Array[String]): Unit = {

        args match {
            case Array(interfaceName, path) =>
                val p = Paths.get(path)
                if (!Files.exists(p) || !Files.isReadable(p)) {
                    println("File not found or not readable!")
                } else {
                    startEmulation(interfaceName, p)
                }
            case _ =>
                println("Usage: <can interface> <config path>")
        }

    }

    def loadConf(path: Path): EmulationSpec = {
        import EmulationConfigurationProtocol._
        Source.fromFile(path.toFile, "UTF-8").mkString.parseYaml.convertTo[EmulationSpec]
    }

    def parseUnsignedHex(n: String): Int = {
        Integer.parseUnsignedInt(n, 16)
    }

//    def positiveSine(period: FiniteDuration): Double => Double = lag(period / 4) andThen sine(period) andThen shift(1) andThen scale(0.5)
//    val coolantTemperature = add(linear(1, 8.minutes), gaussianRandom(-0.001, 0.001)) andThen lowCut(-40) andThen combined(90.0, scale, highCut) andThen shift(40)
//    val engineRpm = add(product(positiveSine(5.minute), square(10.minute) andThen shift(1) andThen scale(0.5)) andThen scale(6000.0), positiveSine(10.minute)) andThen scale(6000.0)

    def startEmulation(interfaceName: String, conf: Path): Unit = {
        val eval = new Eval()
        val emulationSpec = loadConf(conf)
        val t0 = System.currentTimeMillis()

        val controllers = emulationSpec.controllers.map {
            case (hexAddr, ecuSpec) =>
                val addr = parseUnsignedHex(hexAddr)

                val services = ecuSpec.services.map {
                    case (hexSid, serviceSpec) =>
                        val sid = parseUnsignedHex(hexSid)

                        val parameters = serviceSpec.parameters.map {
                            case (hexPid, parameterSpec) =>
                                val pid = parseUnsignedHex(hexPid)

                                (pid, Parameter(parameterSpec.name, pid, new ParameterAction(eval, parameterSpec.action.generator)))
                        }
                        val service = Service(serviceSpec.name, sid, parameters, serviceSpec.action.map(a => new ParameterAction(eval, a.generator)))
                        (sid, service)
                }
                (addr, ECU(ecuSpec.name, addr, services))
        }

        val threadGroup = new ThreadGroup("virtual-ecu-worker-threads")
        val threads: ThreadFactory = r => new Thread(threadGroup, r, "virtual-ecu-worker")
        val socket = NativeRawCanSocket.create()
        socket.bind(interfaceName)
        val broker = new ISOTPBroker(socket, threads, QueueSettings.DEFAULT, ProtocolParameters.DEFAULT, TestDeviceFlowController.INSTANCE)

        val controllerChannels = controllers.mapValues { controller =>
            broker.createChannel(ISOTPAddress.returnAddress(controller.receiveAddress), controller.receiveAddress, aggregateFrames(handleRequest(controller, t0)))
        }

        broker.createChannel(0x000, ISOTPAddress.SFF_FUNCTIONAL_ADDRESS, new FrameHandlerAdapter {
            override def handleSingleFrame(ch: ISOTPChannel, sender: Int, payload: Array[Byte]): Unit = {
                println("Received functional request!")
                for ((addr, controller) <- controllers) {
                    handleRequest(controller, t0)(controllerChannels(addr), addr, payload)
                }
            }
        })

        broker.start()
    }

    def handleRequest(controller: ECU, t0: Long)(ch: ISOTPChannel, sender: Int, payload: Array[Byte]): Unit = {

        val dt = System.currentTimeMillis() - t0

        payload match {
            case Array(sid) =>
                println(s"Controller: ${controller.name}")
                println(s"Service:    ${asHex(sid)}")
                controller.services.get(sid) match {
                    case Some(service) =>
                        service.action match {
                            case Some(action) =>
                                action.execute(dt) match {
                                    case DataResponse(data) =>
                                        val payload = (sid + ObdBridge.PositiveResponseBase).toByte +: data
                                        ch.send(payload)
                                    case Error(msg) =>
                                        println(s"Action failed: $msg")
                                }
                            case None =>
                                println("service cannot be executed directly.")
                        }
                    case None =>
                        println("unknown service!")
                }
            case Array(sid, pids @ _*) =>
                println(s"Controller: ${controller.name}")
                println(s"Service:    ${asHex(sid)}")
                println(s"Parameters: ${hexDump(pids)}")

                controller.services.get(sid) match {
                    case Some(service) =>
                        val unknownPids = (pids.map(_.toInt).toSet -- service.parameters.keySet)
                            .filter(p => (p % ObdBridge.SupportRangeSize) != 0)
                        if (unknownPids.nonEmpty) {
                            println(s"unknown pids: $unknownPids")
                        } else {
                            val successResponseSid = (sid + ObdBridge.PositiveResponseBase).toByte
                            val responses = pids
                                .map(_.toInt)
                                .foldLeft[ActionResult](DataResponse(Array(successResponseSid))) {
                                    case (DataResponse(data), pid) =>
                                        handleRequest(controller, service, pid, dt) match {
                                            case DataResponse(newData) => DataResponse(data ++ (pid.toByte +: newData))
                                            case err => err
                                        }
                                    case (err, _) => err
                                }
                            responses match {
                                case DataResponse(data) =>
                                    ch.send(data)
                                case Error(err) =>
                                    println(s"One of the actions failed: $err")

                            }
                        }
                    case None =>
                        println("Unknown service!")
                }
            case _ =>
                println("Received an empty payload...")
        }

    }

    def handleRequest(controller: ECU, service: Service, pid: Int, dt: Long): ActionResult = {
        if (pid % ObdBridge.SupportRangeSize == 0) {
            handlePidSupportRequest(controller, service, pid)
        } else {
            val parameter = service.parameters(pid)
            parameter.action.execute(dt)
        }
    }

    def handlePidSupportRequest(controller: ECU, service: Service, pid: Int): ActionResult = {
        DataResponse((1 to ObdBridge.SupportRangeSize).foldLeft(BigInt(0)) { (set, n) =>
            if (service.parameters.contains(pid + n)) set | (1 << (32 - n))
            else set
        }.toByteArray)
    }

}
