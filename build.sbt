ThisBuild / organization := "io.github.marcinzh"
ThisBuild / version := "0.46.0"
ThisBuild / scalaVersion := "3.2.1"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ykind-projector:underscores",
  "-Xfatal-warnings",
)

val Deps = {
  object deps {
    val cats_core = "org.typelevel" %% "cats-core" % "2.9.0"
    val specs2_core = "org.specs2" %% "specs2-core" % "5.2.0" % "test"
    val specs2_extra = "org.specs2" %% "specs2-matcher-extra" % "5.2.0" % "test"
    val jol = "org.openjdk.jol" % "jol-core" % "0.16"
  }
  deps
}

lazy val root = project
  .in(file("."))
  .settings(name := "turbolift-root")
  .settings(sourcesInBase := false)
  .settings(dontPublishMe: _*)
  .aggregate(core, extra_effects, devel)

lazy val core = project
  .in(file("modules/core"))
  .settings(name := "turbolift-core")
  .settings(Compile / scalacOptions += "-Yexplicit-nulls")
  .settings(libraryDependencies ++= Seq(
    Deps.cats_core,
    Deps.specs2_core,
    Deps.specs2_extra,
  ))

lazy val extra_effects = project
  .in(file("modules/extra_effects"))
  .settings(name := "turbolift-extra-effects")
  .settings(libraryDependencies ++= Seq(
    Deps.specs2_core,
  ))
  .dependsOn(core)

lazy val devel = project
  .in(file("modules/devel"))
  .settings(name := "turbolift-devel")
  .settings(dontPublishMe: _*)
  .settings(libraryDependencies += Deps.jol)
  .dependsOn(core, extra_effects)

lazy val site = (project in file("docs"))
  .settings(dontPublishMe: _*)
  .settings(moduleName := "site")
  .enablePlugins(MicrositesPlugin)
  .enablePlugins(MdocPlugin)
  .settings(Seq(
    micrositeName := "Turbolift",
    micrositeDescription := "Algebraic Effects for Scala 3",
    micrositeGithubOwner := "marcinzh",
    micrositeGithubRepo := "turbolift",
    micrositeUrl := "https://marcinzh.github.io",
    micrositeBaseUrl := "turbolift",
    micrositeDocumentationUrl := "/turbolift/overview.html",
    micrositeTheme := "pattern",
    micrositeHighlightTheme := "atom-one-light",
    micrositeGitterChannel := false,
    micrositeShareOnSocial := false,
    makeSite / includeFilter := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md",
    mdocIn := (Compile / sourceDirectory).value / "mdoc",
    mdocVariables := Map(
      "VERSION" -> version.value,
    ),
    micrositePalette := Map(
      "brand-primary"     -> "#bd5a10",
      "brand-secondary"   -> "#5c4992",
      "brand-tertiary"    -> "#544184",
      "gray-dark"         -> "#000000",
      "gray"              -> "#333333",
      "gray-light"        -> "#d8d0df",
      "gray-lighter"      -> "#f4f0ff",
      "white-color"       -> "#f8f0ff",
    ),
  ))
  .dependsOn(core)

//=================================================

lazy val dontPublishMe = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

ThisBuild / watchBeforeCommand := Watch.clearScreen
ThisBuild / watchTriggeredMessage := Watch.clearScreenOnTrigger
ThisBuild / watchForceTriggerOnAnyChange := true

Test / parallelExecution := false

//=================================================

ThisBuild / description := "Algebraic Effects for Scala 3"
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
