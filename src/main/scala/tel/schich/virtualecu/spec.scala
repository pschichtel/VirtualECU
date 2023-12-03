package tel.schich.virtualecu

import io.circe.Encoder
import io.circe.derivation.{Configuration, ConfiguredCodec}

given Configuration = Configuration.default.withSnakeCaseMemberNames

case class EmulationSpec(controllers: Map[String, ECUSpec]) derives ConfiguredCodec
case class ECUSpec(name: String, services: Map[String, ServiceSpec]) derives ConfiguredCodec
case class ServiceSpec(name: String, parameters: Map[String, ParameterSpec], action: Option[ActionSpec]) derives ConfiguredCodec
case class ParameterSpec(name: String, action: ActionSpec) derives ConfiguredCodec
case class ActionSpec(generator: Option[String], explicit: Option[String]) derives ConfiguredCodec
case class DeviceAddressSpec(id: String, extended: Boolean) derives ConfiguredCodec
