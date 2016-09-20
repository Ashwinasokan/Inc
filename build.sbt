name := "aiwala"

organization := "edu.colorado.plv"

version := "0.1.0"

description := "Abstract interpreteration for WALA"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.assembla.scala-incubator" %% "graph-core" % "1.10.1",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "com.ibm.wala" % "com.ibm.wala.util" % "1.3.7",
  "com.ibm.wala" % "com.ibm.wala.core" % "1.3.7"
)

// set scalatest options: -o standard output, D durations
testOptions in Test += Tests.Argument("-oD")

// Comment/uncomment to hide/see warnings
scalacOptions ++= Seq("-unchecked", "-deprecation")

// JVM arguments: 8G heap size, 2M stack size
javaOptions ++= Seq("-Xmx8G", "-Xss2M")

// Automatically find main methods from classpath
packAutoSettings