name := "virtualecu"

organization := "tel.schich"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.12.8"

publishMavenStyle := true

resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(
    "tel.schich" %% "obd4s" % "1.0.0-SNAPSHOT",
    "com.beachape" %% "enumeratum" % "1.5.13",   // nicer enums
    "net.jcazevedo" %% "moultingyaml" % "0.4.0", // yaml parser
    "com.twitter" %% "util-eval" % "6.43.0",
)