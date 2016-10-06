name := "lyric-matcher"
 
version := "0.0.1"

scalaVersion := "2.11.1"
 
testOptions in Test += Tests.Argument("-oF")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.7"

libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1101-jdbc41"

libraryDependencies += "com.typesafe.slick" % "slick_2.11" % "2.1.0-M2"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.7"

libraryDependencies += "c3p0" % "c3p0" % "0.9.1.2"

libraryDependencies += "org.jsoup" % "jsoup" % "1.7.3"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9" 

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.9"
