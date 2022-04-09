ThisBuild / organization := "com.github.marcinzh"
ThisBuild / version := "0.21.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.1.1"
ThisBuild / crossScalaVersions := Seq(scalaVersion.value)

ThisBuild / watchBeforeCommand := Watch.clearScreen
ThisBuild / watchTriggeredMessage := Watch.clearScreenOnTrigger
ThisBuild / watchForceTriggerOnAnyChange := true

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ykind-projector:underscores",
  "-Xfatal-warnings",
)

val Deps = {
  object deps {
    val cats_core = libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
    val scalactic = libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.11"
    val scalatest = libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test"
  }
  deps
}

// lazy val commonExceptCoreSettings = Seq(
//   libraryDependencies += compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
// )

lazy val testSettings = Seq(
  Deps.scalactic,
  Deps.scalatest,
  Test / parallelExecution := false,
)

lazy val dontPublishMe = Seq(
  publishTo := None,
  publish := (()),
  publishLocal := (()),
  publishArtifact := false
)

lazy val root = project
  .in(file("."))
  .settings(name := "turbolift-root")
  .settings(sourcesInBase := false)
  .settings(dontPublishMe: _*)
  .aggregate(core)

lazy val core = project
  .in(file("modules/core"))
  .settings(name := "turbolift-core")
  .settings(Deps.cats_core)
  .settings(testSettings: _*)
