name := "javascript-transpiler"

version := "0.1"

scalaVersion := "2.13.1"

// https://mvnrepository.com/artifact/com.lihaoyi/fastparse
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"

// https://mvnrepository.com/artifact/com.github.javaparser/javaparser-core
libraryDependencies += "com.github.javaparser" % "javaparser-core" % "3.15.13"

// https://mvnrepository.com/artifact/junit/junit
libraryDependencies += "junit" % "junit" % "4.13" % Test

// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0-M2" % Test

