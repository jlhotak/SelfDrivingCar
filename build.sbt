enablePlugins(ScalaJSPlugin)

name := "Scala Self Driving Car"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.1"

scalaJSUseMainModuleInitializer := false

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "1.1.0",
  "com.lihaoyi" %%% "scalatags" % "0.9.1",
  "com.lihaoyi" %%% "upickle" % "2.0.0"
)


