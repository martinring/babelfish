enablePlugins(ScalaJSPlugin)

name := "KI-Staging Demo Client"
scalaVersion := "2.12.6" // or any other Scala version >= 2.10.2

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

artifactPath in (Compile, fastOptJS) := file("static/client.js")