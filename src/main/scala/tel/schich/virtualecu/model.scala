package tel.schich.virtualecu

import tel.schich.javacan.IsotpCanChannel
import tel.schich.obd4s.obd.ObdCauses.GeneralReject
import tel.schich.obd4s.{Cause, SimpleCause}

case class ECU(name: String, receiveAddress: Int, services: Map[Int, Service], channel: IsotpCanChannel)
case class Service(name: String, sid: Int, parameters: Map[Int, Parameter], action: Option[Action])
case class Parameter(name: String, id: Int, action: Action)

sealed trait ActionResult
final case class DataResponse(data: Array[Byte]) extends ActionResult
final case class Error(cause: Cause) extends ActionResult

trait Action {
    def execute(t: Long): ActionResult
}

class ParameterAction(compiler: TimeSeriesCompiler, code: String) extends Action {

    private val fullCode =
        s"""
          |import tel.schich.virtualecu.FiniteDurationFunctions._
          |import tel.schich.virtualecu.Quantizers._
          |import scala.concurrent.duration.{DurationDouble, FiniteDuration}
          |
          |$code
        """.stripMargin

    private val f: TimeSeriesScript = compiler(fullCode)

    override def execute(t: Long): ActionResult = {
        try {
            DataResponse(f(t))
        } catch {
            case e: Exception =>
                Error(SimpleCause(e.getLocalizedMessage))
        }
    }
}

object NoOpAction extends Action {
    override def execute(t: Long): ActionResult = Error(GeneralReject)
}



