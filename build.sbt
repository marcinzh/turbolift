val ScalaLTS = "3.3.5"
val ScalaNext = "3.6.3"
ThisBuild / organization := "io.github.marcinzh"
ThisBuild / version := "0.106.0"
ThisBuild / scalaVersion := ScalaLTS
ThisBuild / crossScalaVersions := Seq(ScalaLTS, ScalaNext)
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-release", "11",
)
ThisBuild / javacOptions ++= Seq("--release", "11")
ThisBuild / scalacOptions += (scalaVersion.value match {
  case ScalaLTS => "-Ykind-projector:underscores"
  case ScalaNext => "-Xkind-projector:underscores"
})
ThisBuild / publish / skip := (scalaVersion.value != ScalaLTS)

val Deps = {
  val specs2_v = "5.4.0"
  object deps {
    val specs2_core = "org.specs2" %% "specs2-core" % specs2_v % "test"
    val specs2_extra = "org.specs2" %% "specs2-matcher-extra" % specs2_v % "test"
    val jol = "org.openjdk.jol" % "jol-core" % "0.17"
    val cps = "io.github.dotty-cps-async" %% "dotty-cps-async" % "1.0.0"
    val cps_next = "io.github.dotty-cps-async" %% "dotty-cps-async-next" % "1.0.0"
  }
  deps
}

lazy val root = project
  .in(file("."))
  .settings(name := "turbolift-root")
  .settings(sourcesInBase := false)
  .settings(publish / skip := true)
  .aggregate(core, extra_effects, devel, examples, bindless)

lazy val core = project
  .in(file("modules/core"))
  .settings(name := "turbolift-core")
  .settings(Compile / scalacOptions += "-Yexplicit-nulls")
  .settings(Test / parallelExecution := false)
  .settings(Test / logBuffered := false)
  .settings(libraryDependencies ++= Seq(
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

lazy val examples = project
  .in(file("modules/examples"))
  .settings(name := "turbolift-examples")
  .settings(publish / skip := true)
  .dependsOn(core, extra_effects)

lazy val devel = project
  .in(file("modules/devel"))
  .settings(name := "turbolift-devel")
  .settings(publish / skip := true)
  .settings(libraryDependencies += Deps.jol)
  .dependsOn(core, extra_effects)

lazy val bindless = project
  .in(file("modules/bindless"))
  .settings(name := "turbolift-bindless")
  .settings(publish / skip := false)
  .settings(libraryDependencies += (scalaVersion.value match {
    case ScalaLTS => Deps.cps
    case ScalaNext => Deps.cps_next
  }))
  .settings(moduleName := (scalaVersion.value match {
    case ScalaLTS => "turbolift-bindless"
    case ScalaNext => "turbolift-bindless-next"
  }))
  .settings(libraryDependencies += Deps.specs2_core)
  .settings(licenses += ("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")))
  .dependsOn(core)


lazy val site = (project in file("docs"))
  .settings(publish / skip := true)
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

//-------------------------------------------------

ThisBuild / watchBeforeCommand := Watch.clearScreen
ThisBuild / watchTriggeredMessage := Watch.clearScreenOnTrigger
ThisBuild / watchForceTriggerOnAnyChange := true

Test / parallelExecution := false

val cls = taskKey[Unit]("Clears the console the hard way")

cls := {
  print("\u001b[0m\u001b[2J\u001bc")
}


//-------------------------------------------------

ThisBuild / description := "Algebraic Effects for Scala 3"
ThisBuild / organizationName := "marcinzh"
ThisBuild / homepage := Some(url("https://github.com/marcinzh/turbolift"))
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/marcinzh/turbolift"), "scm:git@github.com:marcinzh/turbolift.git"))
ThisBuild / licenses := List("MIT" -> url("http://www.opensource.org/licenses/MIT"))
ThisBuild / versionScheme := Some("semver-spec")
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
