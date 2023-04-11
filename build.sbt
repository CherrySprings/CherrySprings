val chiselVersion = "3.5.6"

lazy val commonSettings = Seq(
  scalaVersion := "2.13.10",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked"
  ),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "4.0.6"),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.2.14" % "test")
)

lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % chiselVersion,
    "edu.berkeley.cs" %% "chiseltest" % "0.5.5"
  ),
  addCompilerPlugin(("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion).cross(CrossVersion.full))
)

lazy val cde = (project in file("rocket-chip/cde"))
  .settings(commonSettings)
  .settings(
    Compile / scalaSource := baseDirectory.value / "cde/src/chipsalliance/rocketchip"
  )

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
  .dependsOn(cde)
  .dependsOn(hardfloat)
  .dependsOn(rocketMacros)

lazy val difftest = (project in file("difftest"))
  .settings(commonSettings, chiselSettings)

lazy val rocketchipInclusiveCache = (project in file("rocket-chip-inclusive-cache"))
  .settings(commonSettings, chiselSettings)
  .settings(
    Compile / scalaSource := baseDirectory.value / "design/craft/inclusivecache/src"
  )
  .dependsOn(rocketchip)

lazy val cherrysprings = (project in file("."))
  .settings(commonSettings, chiselSettings)
  .dependsOn(rocketchip)
  .dependsOn(cde)
  .dependsOn(difftest)
  .dependsOn(rocketchipInclusiveCache)
