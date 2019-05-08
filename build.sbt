name := "share-buy-sell-criteria-analysis"

version := "1.0"

scalaVersion := "2.10.4"

javaHome := Some(file("C:/Program Files/Java/jdk1.7.0_71"))

//libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"

libraryDependencies += "org.apache.spark" % "spark-core_2.10" % "1.2.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.2" % "test"


net.virtualvoid.sbt.graph.Plugin.graphSettings