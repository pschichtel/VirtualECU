package tel.schich.virtualecu

import java.nio.ByteBuffer
import java.nio.file.{Files, Path, Paths}
import java.time.Duration.ofMinutes
import java.util.concurrent.ThreadFactory

import com.twitter.util.Eval
import net.jcazevedo.moultingyaml._
import tel.schich.javacan.IsotpAddress.SFF_FUNCTIONAL_ADDRESS
import tel.schich.javacan.{CanChannels, IsotpAddress, IsotpCanChannel}
import tel.schich.javacan.select.JavaCANSelectorProvider
import tel.schich.javacan.util.IsotpBroker
import tel.schich.obd4s.ObdBridge
import tel.schich.obd4s.ObdHelper.{asHex, hexDump}

import scala.io.Source
import scala.util.Try

object Main {

    private val writeBuffer = IsotpCanChannel.allocateSufficientMemory()

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

                        val actionLoader = loadAction(eval) _

                        val parameters = serviceSpec.parameters.map {
                            case (hexPid, parameterSpec) =>
                                val pid = parseUnsignedHex(hexPid)

                                val action = actionLoader(parameterSpec.action).getOrElse(NoOpAction)

                                (pid, Parameter(parameterSpec.name, pid, action))
                        }
                        val service = Service(serviceSpec.name, sid, parameters, serviceSpec.action.flatMap(actionLoader))
                        (sid, service)
                }
                (addr, ECU(ecuSpec.name, addr, services))
        }

        val threadGroup = new ThreadGroup("virtual-ecu-worker-threads")
        val threads: ThreadFactory = r => new Thread(threadGroup, r, "virtual-ecu-worker")
        val functionalChannel = CanChannels.newIsotpChannel(interfaceName, SFF_FUNCTIONAL_ADDRESS, 0x7FF)
        val provider = new JavaCANSelectorProvider()
        val broker = new IsotpBroker(threads, provider, ofMinutes(1))

        val controllerChannels = controllers.mapValues { controller =>
            val ch = CanChannels.newIsotpChannel(interfaceName, controller.receiveAddress, IsotpAddress.returnAddress(controller.receiveAddress))
            broker.addChannel(ch, handleRequest(controller, t0))
            ch
        }

        broker.addChannel(functionalChannel, (_, buf) => {
            println("Received functional request!")
            for ((addr, controller) <- controllers) {
                handleRequest(controller, t0)(controllerChannels(addr), buf)
            }
        })

        broker.start()
    }

    def handleRequest(controller: ECU, t0: Long)(ch: IsotpCanChannel, buffer: ByteBuffer): Unit = {

        val dt = System.currentTimeMillis() - t0

        buffer.remaining() match {
            case 1 =>
                val sid = buffer.get()
                println(s"Controller: ${controller.name}")
                println(s"Service:    ${asHex(sid)}")
                controller.services.get(sid) match {
                    case Some(service) =>
                        service.action match {
                            case Some(action) =>
                                action.execute(dt) match {
                                    case DataResponse(data) =>
                                        writeBuffer.clear()
                                        writeBuffer.put((sid + ObdBridge.PositiveResponseBase).toByte)
                                        writeBuffer.put(data)
                                        writeBuffer.flip()

                                        ch.write(writeBuffer)
                                    case Error(msg) =>
                                        println(s"Action failed: $msg")
                                }
                            case None =>
                                println("service cannot be executed directly.")
                        }
                    case None =>
                        println("unknown service!")
                }
            case n if n > 1 =>
                val sid = buffer.get()
                val pids = Array.ofDim[Byte](buffer.remaining())
                buffer.get(pids)
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
                            writeBuffer.clear()
                            writeBuffer.put(successResponseSid)
                            val responses = pids
                                .map(_.toInt)
                                .foldLeft[Either[ActionResult, ByteBuffer]](Right(writeBuffer)) {
                                    case (Right(buf), pid) =>
                                        handleRequest(controller, service, pid, dt) match {
                                            case DataResponse(newData) =>
                                                buf.put(pid.toByte)
                                                buf.put(newData)
                                                Right(buf)
                                            case err => Left(err)
                                        }
                                    case (err, _) => err
                                }
                            responses match {
                                case Right(data) =>
                                    data.flip()
                                    ch.write(data)
                                case Left(Error(err)) =>
                                    println(s"One of the actions failed: $err")
                                case _ =>
                                    println(s"One of the actions failed for an unknown reason!")
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

    def loadAction(eval: Eval)(action: ActionSpec): Option[Action] = {
        action.explicit.flatMap(loadExplicitAction)
            .orElse(action.generator.map(new ParameterAction(eval, _)))
    }

    def loadExplicitAction(className: String): Option[Action] =
        Try(classOf[Action].cast(Class.forName(className).getConstructor().newInstance())).toOption

}
