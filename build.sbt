name := "HackUO"

version := "1.0"

scalaVersion := "2.12.2"

enablePlugins(JavaAppPackaging)

val monocleVersion = "1.5.0"

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging"  %% "scala-logging"  % "3.5.0"
, "ch.qos.logback"              % "logback-classic" % "1.1.8"
, "com.github.scopt"            %% "scopt"          % "3.5.0"
, "org.typelevel"               %% "cats-core"      % "1.0.1"
, "org.bouncycastle"            % "bcprov-jdk15on"  % "1.59"

, "org.scalatest"               %% "scalatest"      % "3.0.3"         % "test"
)

