
ThisBuild / organization := "io.github.jmisabella"
ThisBuild / organizationName := "jmisabella"
ThisBuild / organizationHomepage := Some(url("https://github.com/jmisabella"))
ThisBuild / version := "0.0.27"
ThisBuild / scalaVersion := "2.13.12"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/jmisabella/maze"),
    "scm:git@github.com:jmisabella/maze.git"
  )  
)
ThisBuild / developers := List(
  Developer(
    id = "jmisabella",
    name = "Jeffrey Isabella",
    email = "jeff.isabella@gmail.com",
    url = url("https://github.com/jmisabella")
  )
)
// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}
val javaFxImports = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
    .map(m => "org.openjfx" % s"javafx-$m" % "16" classifier osName)

ThisBuild / libraryDependencies ++= (Seq(
  "com.typesafe.play" %% "play-json" % "2.10.0-RC6",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.13.4",
  "org.scalafx" %% "scalafx" % "16.0.0-R24" % "test",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
) ++ javaFxImports.map(s => s % "test"))
ThisBuild / description := "Scala library for generating and solving mazes"
ThisBuild / licenses := List(
  "Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt")
)
ThisBuild / homepage := Some(url("https://github.com/jmisabella/maze"))
// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

//publishTo := sonatypePublishTo.value
