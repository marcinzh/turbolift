ThisBuild / organization := "com.github.marcinzh"
ThisBuild / version := "0.15.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.1.0"
ThisBuild / crossScalaVersions := Seq(scalaVersion.value)

ThisBuild / watchBeforeCommand := Watch.clearScreen
ThisBuild / watchTriggeredMessage := Watch.clearScreenOnTrigger
ThisBuild / watchForceTriggerOnAnyChange := true

// ThisBuild / resolvers += Resolver.sonatypeRepo("releases")
// ThisBuild / libraryDependencies += Deps.kind_projector

ThisBuild / resolvers += Resolver.jcenterRepo
ThisBuild / credentials += Credentials(Path.userHome / ".bintray" / ".credentials")
ThisBuild / publishTo := Some("Bintray API Realm" at ("https://api.bintray.com/content/marcinzh/maven/turbolift/" ++ version.value))

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ykind-projector:underscores",
  "-Xfatal-warnings",
)

val Deps = {
  object deps {
    val cats_core = libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
    val scalactic = libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10"
    val scalatest = libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
    // val specs2_core = libraryDependencies += ("org.specs2" %% "specs2-core" % "4.12.0" % "test").cross(CrossVersion.for3Use2_13)
    // val specs2_extra = libraryDependencies += ("org.specs2" %% "specs2-matcher-extra" % "4.12.0" % "test").cross(CrossVersion.for3Use2_13)
  }
  deps
}

// lazy val commonExceptCoreSettings = Seq(
//   libraryDependencies += compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
// )

lazy val testSettings = Seq(
  Deps.scalactic,
  Deps.scalatest,
  // Deps.specs2_core,
  // Deps.specs2_extra,
  Test / parallelExecution := false,
  // Test / scalacOptions += "-Yrangepos",
  // Test / scalacOptions += "-language:postfixOps",
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
