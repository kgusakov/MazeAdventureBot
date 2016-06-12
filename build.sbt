name := "MazeAdvenureBot"

lazy val maze = (project in file("maze")).
  settings(Commons.settings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" % "akka-actor_2.11" % "2.4.4"
    )
  ).
  dependsOn(telegramApi)

lazy val telegramApi = (project in file("telegramApi")).
  settings(Commons.settings: _*).
  settings(libraryDependencies ++= Seq(
    "io.argonaut" % "argonaut_2.11" % "6.1",
    "io.megam" % "newman_2.11" % "1.3.12"
  ))
lazy val bot = (project in file("bot")).
  settings(Commons.settings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"
    )
  ).
  dependsOn(maze, telegramApi)