package tel.schich.virtualecu

import net.jcazevedo.moultingyaml._

case class Emulation(controllers: Seq[ECU])
case class ECU(name: String, address: DeviceAddress, services: Map[String, Service])
case class Service(parameters: Map[String, Parameter], action: Option[Action])
case class Parameter(action: Action)
case class Action(generator: String)
case class DeviceAddress(id: String, extended: Boolean)


object EmulationProtocol extends DefaultYamlProtocol {

    implicit val deviceAddressFormat = yamlFormat2(DeviceAddress)
    implicit val actionFormat = yamlFormat1(Action)
    implicit val parameterFormat = yamlFormat1(Parameter)
    implicit val serviceFormat = yamlFormat2(Service)
    implicit val ecuFormat = yamlFormat3(ECU)
    implicit val emulationFormat = yamlFormat1(Emulation)

}

