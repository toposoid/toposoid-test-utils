import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.16" exclude("org.slf4j","slf4j-api")
}
