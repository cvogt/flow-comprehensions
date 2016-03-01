
organization in ThisBuild := "org.scala-comprehensions"
name := "flow-comprehensions"

licenses in ThisBuild := Seq(
  "BSD Simplified" -> url("https://opensource.org/licenses/BSD-2-Clause")
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
