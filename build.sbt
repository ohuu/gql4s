val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "gql",
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
      "org.typelevel" %% "cats-parse" % "0.3.5",
      "org.scalameta" %% "munit"      % "0.7.29" % Test
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    Test / parallelExecution := false
  )
