ThisBuild / organization := "io.github.marcinzh"
ThisBuild / version := "0.27.0"
ThisBuild / scalaVersion := "3.1.1"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ykind-projector:underscores",
  "-Xfatal-warnings",
)

val Deps = {
  object deps {
    val cats_core = "org.typelevel" %% "cats-core" % "2.7.0"
    val scalatest = "org.scalatest" %% "scalatest" % "3.2.11" % "test"
  }
  deps
}

lazy val root = project
  .in(file("."))
  .settings(name := "turbolift-root")
  .settings(sourcesInBase := false)
  .settings(dontPublishMe: _*)
  .aggregate(core)

lazy val core = project
  .in(file("modules/core"))
  .settings(name := "turbolift-core")
  .settings(libraryDependencies ++= Seq(Deps.scalatest, Deps.cats_core))

//=================================================

lazy val dontPublishMe = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

ThisBuild / watchBeforeCommand := Watch.clearScreen
ThisBuild / watchTriggeredMessage := Watch.clearScreenOnTrigger
ThisBuild / watchForceTriggerOnAnyChange := true

//=================================================

ThisBuild / description := "Effect system for Scala"
ThisBuild / organizationName := "marcinzh"
ThisBuild / homepage := Some(url("https://github.com/marcinzh/turbolift"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/marcinzh/turbolift"), "scm:git@github.com:marcinzh/turbolift.git"))
ThisBuild / licenses := List("MIT" -> new URL("http://www.opensource.org/licenses/MIT"))
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  isSnapshot.value match {
    case true => Some("snapshots" at nexus + "content/repositories/snapshots")
    case false => Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}
ThisBuild / pomExtra := (
  <developers>
    <developer>
      <id>marcinzh</id>
      <name>Marcin Żebrowski</name>
      <url>https://github.com/marcinzh</url>
    </developer>
  </developers>
)
