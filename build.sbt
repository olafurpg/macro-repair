lazy val repair = project.settings(
  scalaVersion := "2.12.4",
  libraryDependencies ++= List(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "com.lihaoyi" %% "pprint" % "0.5.2",
    "com.lihaoyi" %% "utest" % "0.5.2" % Test,
    "junit" % "junit" % "4.12" % Test,
    "com.novocode" % "junit-interface" % "0.11" % Test
  )
)
