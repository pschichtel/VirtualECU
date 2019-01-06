package tel.schich.virtualecu

import com.twitter.util.Eval
import net.jcazevedo.moultingyaml._
import tel.schich.javacan.IsotpCanChannel
import tel.schich.obd4s.obd.ObdCauses.GeneralReject
import tel.schich.obd4s.{Cause, SimpleCause}

case class EmulationSpec(controllers: Map[String, ECUSpec])
case class ECUSpec(name: String, services: Map[String, ServiceSpec])
case class ServiceSpec(name: String, parameters: Map[String, ParameterSpec], action: Option[ActionSpec])
case class ParameterSpec(name: String, action: ActionSpec)
case class ActionSpec(generator: Option[String], explicit: Option[String])
case class DeviceAddressSpec(id: String, extended: Boolean)


object EmulationConfigurationProtocol extends DefaultYamlProtocol {

    implicit val deviceAddressFormat = yamlFormat2(DeviceAddressSpec)
    implicit val actionFormat = yamlFormat2(ActionSpec)
    implicit val parameterFormat = yamlFormat2(ParameterSpec)
    implicit val serviceFormat = yamlFormat3(ServiceSpec)
    implicit val ecuFormat = yamlFormat2(ECUSpec)
    implicit val emulationFormat = yamlFormat1(EmulationSpec)

}

case class ECU(name: String, receiveAddress: Int, services: Map[Int, Service], channel: IsotpCanChannel)
case class Service(name: String, sid: Int, parameters: Map[Int, Parameter], action: Option[Action])
case class Parameter(name: String, id: Int, action: Action)

sealed trait ActionResult
final case class DataResponse(data: Array[Byte]) extends ActionResult
final case class Error(cause: Cause) extends ActionResult

trait Action {
    def execute(t: Long): ActionResult
}

class ParameterAction(eval: Eval, code: String) extends Action {

    private val fullCode =
        s"""
          |import tel.schich.virtualecu.FiniteDurationFunctions._
          |import tel.schich.virtualecu.Quantizers._
          |import scala.concurrent.duration.{DurationDouble, FiniteDuration}
          |
          |$code
        """.stripMargin

    private val f: Double => Array[Byte] = eval(fullCode)

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



