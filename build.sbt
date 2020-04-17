scalaVersion := "2.13.1"

val catsVersion           = "2.1.0"
val catsEffectVersion     = "2.1.2"
val specs2Version         = "4.8.3"
val logbackVersion        = "1.2.3"
val scalacheckVersion     = "1.14.1"
val catsEffectTestVersion = "0.3.0"

lazy val root = (project in file("."))
  .settings(
    organization := "com.minosiatns",
    name := "benc",
    scalaVersion := "2.13.1",
    libraryDependencies ++= Seq(
      "org.typelevel"   %% "cats-core"                  % catsVersion,
      "org.typelevel"   %% "cats-effect"                % catsEffectVersion,
      "org.scalacheck"  %% "scalacheck"                 % scalacheckVersion % "test",
      "com.codecommit"  %% "cats-effect-testing-specs2" % catsEffectTestVersion % "test",
      "ch.qos.logback"  % "logback-classic"             % logbackVersion
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
  ).settings(releaseProcessSettings)

import ReleaseTransformations._
lazy val releaseProcessSettings = Seq(
  releaseIgnoreUntrackedFiles := true,
  releaseProcess := Seq[ReleaseStep](checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    pushChanges))



