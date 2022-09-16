lazy val root = project
  .in(file("."))
  .settings(
    name                := "sclin",
    version             := "0.0.0",
    scalaVersion        := "3.2.0",
    libraryDependencies += "org.scalameta" %% "munit"     % "1.0.0-M6" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0",
    libraryDependencies += "org.typelevel" %% "spire"     % "0.18.0",
    libraryDependencies += "com.lihaoyi"   %% "mainargs"  % "0.2.3",
    libraryDependencies += "com.lihaoyi"   %% "os-lib"    % "0.8.0",
    libraryDependencies += "com.lihaoyi"   %% "fansi"     % "0.4.0"
  )
