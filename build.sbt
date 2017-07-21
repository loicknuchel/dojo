name := "dojo"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies ++= List(
  "com.criteo.lolhttp" %% "lolhttp" % "0.4.1",
  "com.criteo.lolhttp" %% "loljson" % "0.4.1",
  "com.criteo.lolhttp" %% "lolhtml" % "0.4.1",
  "com.typesafe.akka" %% "akka-http" % "10.0.4",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
