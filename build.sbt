val scala3Version = "3.1.0"

lazy val catsParseVersion = "0.3.6"
lazy val munitVersion     = "0.7.29"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "gql4s",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-encoding",
      "utf-8",        // Specify character encoding used by source files.
      "-deprecation", // Emit warning and location for usages of deprecated APIs.
      "-feature", // Emit warning and location for usages of features that should be imported explicitly.
      "-unchecked",     // Enable additional warnings where generated code depends on assumptions.
      "-explain",       // Explain errors in more detail
      "-explain-types", // Explain type errors in more detail
      "-indent",        // Allow significant indentation.
      "-new-syntax"     // Require `then` and `do` in control expressions.
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % catsParseVersion,
      "org.scalameta" %% "munit"      % munitVersion % Test
    ),
    // testFrameworks += new TestFramework("munit.Framework"),
    Test / parallelExecution := false
  )
