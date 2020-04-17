scalaVersion := "2.13.1"

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
    scalaVersion := "2.13.1",
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "cats-core"         % catsVersion,
      "com.chuusai"    %% "shapeless"         % shaplessVersion,
      "org.scodec"     %% "scodec-core"       % scodecCoreVersion,
      "org.scodec"     %% "scodec-cats"       % scodecCatsVersion,
      "org.scalacheck" %% "scalacheck"        % scalacheckVersion % Test,
      "org.specs2"     %% "specs2-core"       % specs2Version % Test,
      "org.specs2"     %% "specs2-scalacheck" % specs2Version % Test
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
  )
  .settings(releaseProcessSettings)

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
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    pushChanges
  )
)
