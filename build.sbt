lazy val root = (project in file(".")).
  settings(
      name := "PsddStrucutreLearning",
      version := "1.0",
      scalaVersion := "2.11.8",
      scalacOptions ++= Seq("-unchecked", "-deprecation", "-target:jvm-1.6","-Xdisable-assertions","-optimise"),
      assemblyJarName in assembly := "PSDD.jar",
      test in assembly := {},
      mainClass in assembly := Some ("main.Main"),
      assemblyJarName in assembly := "psdd.jar",
      libraryDependencies += "com.github.scopt" %% "scopt" % "3.5.0",
      libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test->default",
      libraryDependencies += "com.google.guava" % "guava" % "19.0",
      libraryDependencies += "com.typesafe" % "config" % "1.2.1",
      libraryDependencies += "org.spire-math" %% "spire" % "0.13.0",
      resolvers += Resolver.sonatypeRepo("public")
     )
