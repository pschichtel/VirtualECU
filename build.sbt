name := "virtualecu"

organization := "tel.schich"

version := "1.0.0-SNAPSHOT"

scalaVersion := "3.6.2"

publishMavenStyle := true

resolvers += Resolver.mavenLocal

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
    "tel.schich" %% "obd4s" % "1.1.0" changing(),
    "tel.schich" % "javacan-core-arch-detect" % "3.5.0",
    "tel.schich" % "javacan-epoll-arch-detect" % "3.5.0",
    "io.circe" %% "circe-yaml" % "1.15.0",
    "org.slf4j" % "slf4j-simple" % "2.0.16",
    scalaOrganization.value %% "scala3-compiler" % scalaVersion.value,
)