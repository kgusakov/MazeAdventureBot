import Commons.Versions._

name := "MazeAdvenureBot"

lazy val maze = (project in file("maze")).
  settings(Commons.settings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % akkaVersion
    )
  ).
  dependsOn(telegramApi)

lazy val telegramApi = (project in file("telegramApi")).
  settings(Commons.settings: _*).
  settings(libraryDependencies ++= Seq(
    "io.argonaut" %% "argonaut" % "6.1",
    "com.typesafe.akka" %% "akka-http-core" % akkaVersion
  ))

lazy val bot = (project in file("bot")).
  settings(Commons.settings: _*).
  settings(libraryDependencies ++= Seq(
    "com.jsuereth" %% "scala-arm" % "1.4"
  )).
  dependsOn(maze, telegramApi)

mainClass in assembly := Some("com.maze.bot.Bot")