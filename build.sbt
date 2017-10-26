lazy val repair = project.settings(
  scalaVersion := "2.12.4",
  libraryDependencies ++= List(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "junit" % "junit" % "4.12" % Test,
    "com.novocode" % "junit-interface" % "0.11" % Test
  )
)
