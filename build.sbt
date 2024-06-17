val scala3Version = "3.4.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "task",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq ("org.scalameta" %% "munit" % "1.0.0" % Test,
     "com.lihaoyi" %% "upickle" % "3.0.0",
     "com.typesafe.play" %% "play-json" % "2.10.0-RC6")
 )
