val chiselVersion = "3.5.5"
scalaVersion := "2.12.16"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-unchecked",
    "-feature",
    "-Xsource:2.11"
  ),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "4.0.6"),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.14" % "test"),
  addCompilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full))
)

lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % chiselVersion,
    "edu.berkeley.cs" %% "chiseltest" % "0.5.5"
  ),
  addCompilerPlugin(("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion).cross(CrossVersion.full))
)

lazy val `api-config-chipsalliance` = (project in file("rocket-chip/api-config-chipsalliance/build-rules/sbt"))
  .settings(commonSettings)

lazy val hardfloat = (project in file("rocket-chip/hardfloat"))
  .settings(commonSettings, chiselSettings)

lazy val rocketMacros = (project in file("rocket-chip/macros"))
  .settings(commonSettings)

lazy val rocketchip = (Project("rocket-chip", file("rocket-chip/src")))
  .settings(commonSettings, chiselSettings)
  .settings(
    Compile / scalaSource       := baseDirectory.value / "main" / "scala",
    Compile / resourceDirectory := baseDirectory.value / "main" / "resources"
  )
  .dependsOn(`api-config-chipsalliance`)
  .dependsOn(hardfloat)
  .dependsOn(rocketMacros)

lazy val difftest = (project in file("difftest"))
  .settings(commonSettings, chiselSettings)

lazy val huancun = (project in file("HuanCun"))
  .settings(commonSettings, chiselSettings)
  .dependsOn(rocketchip)

lazy val cherrysprings = project
  .in(file("."))
  .settings(commonSettings, chiselSettings)
  .dependsOn(rocketchip)
  .dependsOn(difftest)
  .dependsOn(huancun)
