package tel.schich.virtualecu

import net.jcazevedo.moultingyaml._

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