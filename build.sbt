val scala3Version = "3.9.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fun_jvm",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.7.1",
    libraryDependencies += "co.fs2" %% "fs2-core" % "3.14.0"
  )
