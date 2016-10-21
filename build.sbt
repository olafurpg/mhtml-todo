enablePlugins(ScalaJSPlugin)

name := "Scala.js Tutorial"

scalaVersion := "2.11.8" // or any other Scala version >= 2.10.2

libraryDependencies += "in.nvilla" %%% "monadic-rx-cats" % "0.1"
libraryDependencies += "in.nvilla" %%% "monadic-html" % "0.1"
libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.3.9"
