sourcesInBase := false

lazy val commonSettings = Seq(
  organization := "com.github.marcinzh",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.10",
  crossScalaVersions := Seq(scalaVersion.value),
  scalacOptions ++= Seq(
    "-language:implicitConversions",
    "-language:higherKinds",
    "-unchecked", 
    "-feature", 
    "-deprecation",
    "-Ypartial-unification",
    // "-Ywarn-unused:imports,privates,-patvars,-locals,params,-implicits"
    // "-Ywarn-unused:privates,-patvars,-locals,params,-implicits"
  ),
  resolvers += Resolver.sonatypeRepo("releases"),
  libraryDependencies += compilerPlugin("org.typelevel" % "kind-projector" % "0.10.0" cross CrossVersion.binary),
)

lazy val commonExceptCoreSettings = Seq(
  libraryDependencies += compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
)

lazy val testSettings = Seq(
  libraryDependencies += "org.specs2" %% "specs2-core" % "4.3.4" % "test",
  libraryDependencies += "org.specs2" %% "specs2-matcher-extra" % "4.3.4" % "test",
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
  .settings(commonSettings: _*)
  .settings(dontPublishMe: _*)
  .aggregate(core, mwords)

lazy val core = project
  .in(file("modules/core"))
  .settings(name := "turbolift-core")
  .settings(commonSettings: _*)
  .settings(testSettings: _*)
  .dependsOn(mwords)

lazy val mwords = project
  .in(file("modules/mwords"))
  .settings(name := "turbolift-mwords")
  .settings(commonSettings: _*)
  .settings(commonExceptCoreSettings: _*)
