import akka.actor.{Actor, Stash}
import com.maze.bot.telegram.api.{SendMessage, TelegramApiClient}

import scala.util.Random

sealed trait Wall {
  object Up extends Wall
  object Down extends Wall
  object Left extends Wall
  object Right extends Wall
}

sealed trait Item
object Exit extends Item

sealed trait Direction {
  object Up extends Direction
  object Down extends Direction
  object Left extends Direction
  object Right extends Direction
}

case class Cell(walls: Set[Wall], item: Item)

case class Maze(cells: Array[Array[Cell]], players: Set[Player])

case class Player(id: Int, position: (Int, Int))

case class Move(playerId: Int, direction: Direction)
case class MoveResult(player: Player, item: Option[Item])
case class Init(ids: Set[Int])

case class Game(playerIds: Set[Int]) {

  val maze = generateMaze((10, 10), playerIds)

  private def generateMaze(dimension: (Int, Int), playerIds: Set[Int]): Maze = {
    val r = Random
    Maze(
      Array.ofDim(dimension._1, dimension._2),
      playerIds.map(id => Player(id,
        (r.nextInt(dimension._1), r.nextInt(dimension._2)))))
  }
}

class MazeActor extends Actor with Stash {

  private var maze: Option[Maze] = None
  private var players: Set[Player] = Set.empty

  override def receive: Receive = {
    case Init(ids) =>
      maze = Some(generateMaze((10, 10), ids))
      context.become(active)
      unstashAll();
    case _ => stash()
  }

  val active: Receive = {
    case Move(playerId, direction) => {
      val actionResults = List("Wall", "Ok", "Chest")
      actionResults(Random.nextInt(actionResults.size))
      TelegramApiClient.sendMessage(SendMessage(playerId.chatId, actionResults(Random.nextInt(actionResults.size))))
    }
  }


}

class Game extends Actor {
  override def receive: Actor.Receive = ???
}



