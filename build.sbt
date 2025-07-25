val scala3Version = "3.6.4"

scalacOptions ++= Seq("--add-exports", "java.base/jdk.internal.vm=ALL-UNNAMED")
run / fork := true
run / javaOptions ++= Seq("--add-opens", "java.base/jdk.internal.vm=ALL-UNNAMED", "--sun-misc-unsafe-memory-access=allow")

lazy val root = project
  .in(file("."))
  .settings(
    name := "Direct Style Effects Lab",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
