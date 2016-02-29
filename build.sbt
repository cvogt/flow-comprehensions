
scalaVersion := "2.11.7"

val EmmVersion = "0.2.1"
val ScalazVersion = "7.2.0"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "compile",
  "org.scalaz" %% "scalaz-concurrent" % ScalazVersion % Test,
  "org.scalaz" %% "scalaz-concurrent" % ScalazVersion % Test,
  "com.codecommit" %% "emm-core" % EmmVersion % Test,
  "com.codecommit" %% "emm-scalaz-72" % EmmVersion % Test
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

scalacOptions := List("-feature", "-deprecation")
