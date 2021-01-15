ThisBuild / organization := "com.github.marcinzh"
ThisBuild / version := "0.8.0"
ThisBuild / scalaVersion := "2.13.4"
ThisBuild / crossScalaVersions := Seq(scalaVersion.value)

ThisBuild / watchBeforeCommand := Watch.clearScreen
ThisBuild / watchTriggeredMessage := Watch.clearScreenOnTrigger
ThisBuild / watchForceTriggerOnAnyChange := true

ThisBuild / resolvers += Resolver.sonatypeRepo("releases")
ThisBuild / libraryDependencies += Deps.kind_projector

ThisBuild / resolvers += Resolver.jcenterRepo
ThisBuild / credentials += Credentials(Path.userHome / ".bintray" / ".credentials")
ThisBuild / publishTo := Some("Bintray API Realm" at ("https://api.bintray.com/content/marcinzh/maven/turbolift/" ++ version.value))

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xfatal-warnings",
  // "-Ykind-projector",
)

val Deps = {
  object deps {
    val kind_projector = compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)
    val better_for = libraryDependencies += compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")
    val cats_core = libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0"
    val specs2_core = libraryDependencies += "org.specs2" %% "specs2-core" % "4.10.0" % "test"
    val specs2_extra = libraryDependencies += "org.specs2" %% "specs2-matcher-extra" % "4.10.0" % "test"
  }
  deps
}

// lazy val commonExceptCoreSettings = Seq(
//   libraryDependencies += compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
// )

lazy val testSettings = Seq(
  Deps.specs2_core,
  Deps.specs2_extra,
  parallelExecution in Test := false,
  scalacOptions in Test += "-Yrangepos",
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
