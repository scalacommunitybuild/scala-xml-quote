lazy val commonSettings = Seq(
  version := "0.1.4",
  scalaVersion := "2.12.16",
  crossScalaVersions := Seq("2.12.16", "2.11.11"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xlint"
  ),
  // convenient when working with macros
  clean in Test := IO.delete((classDirectory in Test).value)
)

lazy val bintrayPublishSettings = Seq(
  licenses += "BSD New" -> url("http://opensource.org/licenses/BSD-3-Clause"),
  organization := "org.renucci"
)

lazy val xmlquote = (project in file("."))
  .settings(
    commonSettings,
    bintrayPublishSettings,
    name := "scala-xml-quote",
    initialCommands in console := "import scala.xml.quote._",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
      "com.lihaoyi" %% "fastparse" % "0.4.3",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    )
  )
