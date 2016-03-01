
organization in ThisBuild := "org.scala-comprehensions"
name := "flow-comprehensions"

licenses in ThisBuild := Seq(
  "BSD Simplified" -> url("https://opensource.org/licenses/BSD-2-Clause")
)

developers in ThisBuild := List(
  Developer(
    id = "cvogt",
    name = "Chris Vogt",
    email = "cvogt@cvogt.org",
    url = url("http://cvogt.org")
  ),
  Developer(
    id = "clhodapp",
    name = "Chris Hodapp",
    email = "clhodapp1@gmail.com",
    url = url("https://github.com/clhodapp")
  )
)

scalaVersion in ThisBuild := "2.11.7"

val EmmVersion = "0.2.1"
val ScalazVersion = "7.2.0"
libraryDependencies in ThisBuild ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "compile",
  "org.scalaz" %% "scalaz-concurrent" % ScalazVersion % Test,
  "org.scalaz" %% "scalaz-concurrent" % ScalazVersion % Test,
  "com.codecommit" %% "emm-core" % EmmVersion % Test,
  "com.codecommit" %% "emm-scalaz-72" % EmmVersion % Test
)

bintrayOrganization in ThisBuild := Some("scala-comprehensions")
publishMavenStyle in ThisBuild := true
scmInfo in ThisBuild := Some(
  ScmInfo(
    browseUrl = url("https://github.com/cvogt/flow-comprehensions"),
    connection = "ssh://git@github.com:/cvogt/flow-comprehensions.git"
  )
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

scalacOptions in ThisBuild := List("-feature", "-deprecation")

releaseTagComment := s"Releasing ${(version in ThisBuild).value}"
releaseCommitMessage := {
  if (isSnapshot.value) s"Setting version to ${version.value} for next development iteration"
  else s"Setting version to ${version.value} for release"
}

releasePublishArtifactsAction := PgpKeys.publishSigned.value
