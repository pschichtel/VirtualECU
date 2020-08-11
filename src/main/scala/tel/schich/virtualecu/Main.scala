package tel.schich.virtualecu

import java.nio.ByteBuffer
import java.nio.file.{Files, Path, Paths}
import java.time.Duration.ofMinutes
import java.util.concurrent.ThreadFactory

import net.jcazevedo.moultingyaml._
import tel.schich.javacan.IsotpAddress.SFF_FUNCTIONAL_ADDRESS
import tel.schich.javacan._
import tel.schich.javacan.linux.epoll.EPollSelector
import tel.schich.javacan.util.IsotpListener
import tel.schich.obd4s.{Cause, Identified, ObdBridge}
import tel.schich.obd4s.ObdHelper.{asHex, hexDump}
import tel.schich.obd4s.obd.ObdCauses

import scala.io.Source
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.currentMirror
import scala.util.{Failure, Success, Try, Using}

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
                    loadConf(p) match {
                        case Success(spec) =>
                            startEmulation(NetworkDevice.lookup(interfaceName), spec)
                        case Failure(e) =>
                        System.err.println("Failed to load the spec:")
                        e.printStackTrace(System.err)
                    }
                }
            case _ =>
                println("Usage: <can interface> <config path>")
        }

    }

    def loadConf(path: Path): Try[EmulationSpec] = {
        import EmulationConfigurationProtocol._
        Using(Source.fromFile(path.toFile, "UTF-8"))(_.mkString.parseYaml.convertTo[EmulationSpec])
    }

    def parseUnsignedHex(n: String): Int = {
        Integer.parseUnsignedInt(n, 16)
    }

    def compileScript(code: String): TimeSeriesScript = {
        val toolbox = currentMirror.mkToolBox()

        val result = toolbox.eval(toolbox.parse(code))
        result match {
            case script: TimeSeriesScript @unchecked => script
            case _ => throw new IllegalArgumentException("Failed to compile script!")
        }
    }

    def startEmulation(device: NetworkDevice, emulationSpec: EmulationSpec): Unit = {
        val t0 = System.currentTimeMillis()

        val controllers = emulationSpec.controllers.map {
            case (hexAddr, ecuSpec) =>
                val addr = parseUnsignedHex(hexAddr)

                val services = ecuSpec.services.map {
                    case (hexSid, serviceSpec) =>
                        val sid = parseUnsignedHex(hexSid)

                        val actionLoader = loadAction(compileScript) _

                        val parameters = serviceSpec.parameters.map {
                            case (hexPid, parameterSpec) =>
                                val pid = parseUnsignedHex(hexPid)

                                val action = actionLoader(parameterSpec.action).getOrElse(NoOpAction)

                                (pid, Parameter(parameterSpec.name, pid, action))
                        }
                        val service = Service(serviceSpec.name, sid, parameters, serviceSpec.action.flatMap(actionLoader))
                        (sid, service)
                }
                val ch = CanChannels.newIsotpChannel(device, addr, IsotpAddress.returnAddress(addr))
                ch.configureBlocking(false)
                (addr, ECU(ecuSpec.name, addr, services, ch))
        }

        val threadGroup = new ThreadGroup("virtual-ecu-worker-threads")
        val threads: ThreadFactory = r => new Thread(threadGroup, r, "virtual-ecu-worker")
        val functionalChannel = CanChannels.newIsotpChannel(device, SFF_FUNCTIONAL_ADDRESS, 0x7FF)
        val listener = new IsotpListener(threads, EPollSelector.PROVIDER, ofMinutes(1))

        controllers.values.foreach { controller =>
            listener.addChannel(controller.channel, handleRequest(controller.name, controller, t0) _)
        }

        listener.addChannel(functionalChannel, (_, buf) => {
            println("Received functional request!")
            for (controller <- controllers.values) {
                handleRequest("all", controller, t0)(controller.channel, buf)
            }
        })

        println("Virtual ECUs started!")
    }

    def handleRequest(name: String, controller: ECU, t0: Long)(ch: IsotpCanChannel, buffer: ByteBuffer): Unit = {

        println("#######################")
        println(s"ECU: $name")

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
                                writeErrorResponse(ch, sid, ObdCauses.ServiceNotSupported)
                                println("service cannot be executed directly.")
                        }
                    case None =>
                        writeErrorResponse(ch, sid, ObdCauses.ServiceNotSupported)
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
                            writeErrorResponse(ch, sid, ObdCauses.SubFunctionNotSupportedInvalidFormat)
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
                                case Left(Error(cause: Identified)) if ObdCauses.values.contains(cause) =>
                                    writeErrorResponse(ch, sid, cause)
                                case Left(Error(err)) =>
                                    writeErrorResponse(ch, sid, ObdCauses.RequestSequenceError)
                                    println(s"One of the actions failed: ${err.reason}")
                                case _ =>
                                    writeErrorResponse(ch, sid, ObdCauses.GeneralReject)
                                    println(s"One of the actions failed for an unknown reason!")
                            }
                        }
                    case None =>
                        writeErrorResponse(ch, sid, ObdCauses.ServiceNotSupported)
                        println("Unknown service!")
                }
            case _ =>
                println("Received an empty payload...")
        }

    }

    private def writeErrorResponse(ch: IsotpCanChannel, sid: Byte, cause: Cause with Identified): Unit = {
        writeBuffer.clear()
        writeBuffer.put(ObdCauses.NegativeResponseCode)
        writeBuffer.put(sid)
        writeBuffer.put(cause.id.toByte)
        writeBuffer.flip()
        ch.write(writeBuffer)
    }

    def handleRequest(controller: ECU, service: Service, pid: Int, dt: Long): ActionResult = {
        if (pid % ObdBridge.SupportRangeSize == 0) {
            handlePidSupportRequest(controller, service, pid)
        } else {
            val parameter = service.parameters(pid)
            println(s"PID request: ${parameter.name}")
            parameter.action.execute(dt)
        }
    }

    def isSupportPidNeeded(service: Service, pid: Int): Boolean = {
        if (pid % ObdBridge.SupportRangeSize != 0) false
        else service.parameters.keys.exists(_ > pid)
    }

    def handlePidSupportRequest(controller: ECU, service: Service, pid: Int): ActionResult = {
        println(s"Support PID request: ${pid.toHexString.reverse.padTo(2, '0').reverse}")
        if (isSupportPidNeeded(service, pid)) {
            val mask = (1 to ObdBridge.SupportRangeSize).foldLeft(BigInt(0)) { (set, n) =>
                if (service.parameters.contains(pid + n)) set | (1 << (32 - n))
                else set
            }
            val finalMask =
                if (isSupportPidNeeded(service, pid + ObdBridge.SupportRangeSize)) mask | 1
                else mask
            val intBytes = finalMask.toByteArray
            val result =
                if (intBytes.length == 4) intBytes
                else {
                    val resultBuf = Array.ofDim[Byte](4)
                    System.arraycopy(intBytes, 0, resultBuf, resultBuf.length - intBytes.length, intBytes.length)
                    resultBuf
                }
            DataResponse(result)
        } else {
            Error(ObdCauses.RequestSequenceError)
        }
    }

    def loadAction(compiler: TimeSeriesCompiler)(action: ActionSpec): Option[Action] = {
        action.explicit.flatMap(loadExplicitAction)
            .orElse(action.generator.map(new ParameterAction(compiler, _)))
    }

    def loadExplicitAction(className: String): Option[Action] =
        Try(classOf[Action].cast(Class.forName(className).getConstructor().newInstance())).toOption

}
