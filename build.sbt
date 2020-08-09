name := "virtualecu"

organization := "tel.schich"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.13.3"

publishMavenStyle := true

resolvers += Resolver.mavenLocal

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
    "tel.schich" %% "obd4s" % "1.0.0",
    "com.beachape" %% "enumeratum" % "1.5.15",   // nicer enums
    "net.jcazevedo" %% "moultingyaml" % "0.4.1", // yaml parser
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
)