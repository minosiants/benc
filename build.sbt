val catsVersion       = "2.1.0"
val shaplessVersion   = "2.3.3"
val specs2Version     = "4.8.3"
val scodecCoreVersion = "1.11.6"
val scodecCatsVersion = "1.0.0"
val scalacheckVersion = "1.14.1"

lazy val root = (project in file("."))
  .settings(
    organization := "com.minosiatns",
    name := "benc",
    scalaVersion := "2.12.11",
    crossScalaVersions := Seq("2.12.11", "2.13.1"),
    scalacOptions ++= Seq(
      "-language:experimental.macros",
      "-Yrangepos",
      "-Ywarn-unused",
      "-Xlint"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "cats-core"         % catsVersion,
      "com.chuusai"    %% "shapeless"         % shaplessVersion,
      "org.scodec"     %% "scodec-core"       % scodecCoreVersion,
      "org.scodec"     %% "scodec-cats"       % scodecCatsVersion,
      "org.scalacheck" %% "scalacheck"        % scalacheckVersion % Test,
      "org.specs2"     %% "specs2-core"       % specs2Version % Test,
      "org.specs2"     %% "specs2-scalacheck" % specs2Version % Test
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
    addCompilerPlugin(scalafixSemanticdb)
  )
  .settings(licenceSettings)
  .settings(releaseProcessSettings)

lazy val licenceSettings = Seq(
  organizationName := "Kaspar Minosiants",
  startYear := Some(2020),
  licenses += ("Apache-2.0", new URL(
    "https://www.apache.org/licenses/LICENSE-2.0.txt"
  ))
)

publishTo := sonatypePublishToBundle.value

import ReleaseTransformations._
lazy val releaseProcessSettings = Seq(
  releaseIgnoreUntrackedFiles := true,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    releaseStepCommandAndRemaining("+publishSigned"),
    releaseStepCommand("sonatypeBundleRelease"),
    setNextVersion,
    commitNextVersion,
    pushChanges
  )
)
