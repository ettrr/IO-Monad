ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "IO-Monad"
  )


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19",
  "org.typelevel" %% "cats-effect"          % "3.5.5",
  "org.typelevel" %% "cats-laws"            % "2.12.0" % Test,
  "org.typelevel" %% "discipline-scalatest" % "2.3.0"  % Test
)