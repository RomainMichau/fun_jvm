val scala3Version = "3.9.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fun_jvm",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    // Non-exhaustive pattern matches are a compile error, not just a warning - a missing case
    // on a sealed hierarchy is exactly the kind of bug (see FValueClassRef(None) in
    // MethodExecutor.isAssignable) that's cheap to catch here and expensive to hit at runtime.
    scalacOptions += "-Wconf:msg=match may not be exhaustive:error",

    libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.7.1",
    libraryDependencies += "co.fs2" %% "fs2-core" % "3.14.0",
    libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.17",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.17"
  )
