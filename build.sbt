name := "Appia2"

scalaVersion := "2.10.3"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.1"

libraryDependencies += "com.typesafe" %% "scalalogging-slf4j" % "1.1.0"

libraryDependencies += "com.typesafe.slick" %% "slick" % "2.0.1"

libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1101-jdbc41"

libraryDependencies += "net.sf.opencsv" % "opencsv" % "2.3"

libraryDependencies += "org.jsoup" % "jsoup" % "1.7.3"

libraryDependencies += "joda-time" % "joda-time" % "2.3"

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2"

scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature" )
