import Dependencies._
import de.heikoseeberger.sbtheader.License

ThisBuild / scalaVersion     := "3.3.6"
ThisBuild / version          := "0.7-SNAPSHOT"
ThisBuild / organization     := "com.ideal.linked"
val PekkoVersion = "1.1.5"

lazy val root = (project in file("."))
  .settings(
    name := "toposoid-test-utils",    
    libraryDependencies += "com.ideal.linked" %% "scala-common" % "0.7-SNAPSHOT" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "com.ideal.linked" %% "toposoid-knowledgebase-model" % "0.7-SNAPSHOT" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "com.ideal.linked" %% "toposoid-deduction-protocol-model" % "0.7-SNAPSHOT"exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "com.ideal.linked" %% "toposoid-common" % "0.7-SNAPSHOT" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "com.ideal.linked" %% "toposoid-sentence-transformer-neo4j" % "0.7-SNAPSHOT" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "com.ideal.linked" %% "toposoid-feature-vectorizer" % "0.7-SNAPSHOT" exclude("org.slf4j","slf4j-api"),    
    libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.13" exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "org.apache.pekko" %% "pekko-actor-typed" % PekkoVersion exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "org.apache.pekko" %% "pekko-serialization-jackson" % PekkoVersion exclude("org.slf4j","slf4j-api"),
    libraryDependencies += "org.apache.pekko" %% "pekko-slf4j" % PekkoVersion exclude("org.slf4j","slf4j-api"),
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.36" , 
    parallelExecution in Test := false
    
  )
  .enablePlugins(AutomateHeaderPlugin)

organizationName := "Linked Ideal LLC.[https://linked-ideal.com/]"
startYear := Some(2021)
licenses += ("AGPL-3.0-or-later", url("http://www.gnu.org/licenses/agpl-3.0.en.html"))
headerLicense := Some(License.AGPLv3("2025", organizationName.value))

