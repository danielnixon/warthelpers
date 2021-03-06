import scalariform.formatter.preferences._

val scala210 = "2.10.6"
val scala211 = "2.11.11"
val scala212 = "2.12.2"
val wartremoverVersion = "2.1.0"
val coursierVersion = "1.0.0-RC3"

lazy val commonSettings = Seq(
  organization := "org.danielnixon",
  licenses := Seq("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  version := "0.4.0-SNAPSHOT",
  publishMavenStyle := true,
  publishArtifact in Test := false,
  publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  homepage := Some(url("https://github.com/danielnixon/warthelpers")),
  pomExtra := {
    <scm>
      <url>git@github.com:danielnixon/warthelpers.git</url>
      <connection>scm:git:git@github.com:danielnixon/warthelpers.git</connection>
    </scm>
      <developers>
        <developer>
          <id>danielnixon</id>
          <name>Daniel Nixon</name>
          <url>https://danielnixon.org/</url>
        </developer>
      </developers>
  },
  scalariformPreferences := scalariformPreferences.value
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings",
    "-Ywarn-dead-code",
    "-Ywarn-inaccessible",
    "-Ywarn-value-discard",
    "-Ywarn-numeric-widen",
    "-Ywarn-nullary-override")
)

lazy val wartHelpers = Project(
  id = "wart-helpers",
  base = file("wart-helpers")
).settings(commonSettings ++ Seq(
  scalaVersion := scala211,
  crossScalaVersions := Seq(scala211, scala212),
  libraryDependencies ++= Seq(
    "org.wartremover" %% "wartremover" % wartremoverVersion
  ),
  scalacOptions ++= Seq("-Xlint:_", "-Ywarn-unused", "-Ywarn-unused-import")
): _*)

lazy val sbtPluginHelpers: Project = Project(
  id = "wart-helpers-sbt",
  base = file("wart-helpers-sbt")
).settings(commonSettings ++ Seq(
  scalaVersion := scala210,
  addSbtPlugin("org.wartremover" %% "sbt-wartremover" % wartremoverVersion),
  libraryDependencies ++= Seq(
    "io.get-coursier" %% "coursier" % coursierVersion,
    "io.get-coursier" %% "coursier-cache" % coursierVersion
  ),
  scalacOptions += "-Xlint"
): _*)
