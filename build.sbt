name := "MazeAdvenureBot"

lazy val maze = (project in file("maze")).
  settings(Commons.settings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % "2.4.4"
    )
  )