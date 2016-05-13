import akka.actor.{Stash, Actor}
import akka.actor.Actor.Receive

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

case class Player(id: Long, position: (Int, Int))

case class Move(playerId: Long, direction: Direction)
object Init

class MazeActor(playersIds: Set[Long]) extends Actor with Stash {

  private var maze: Option[Maze] = None

  @throws[Exception](classOf[Exception])
  override def preStart() {
    super.preStart()
    self ! Init
  }

  override def receive: Receive = {
    case Init =>
      maze = Some(generateMaze((10, 10), playersIds))
      context.become(active)
      unstashAll();
    case _ => stash()
  }

  val active: Receive = {
    case Move(playerId, direction) => {
      maze.map { m =>
        m.players.find(p => p.id == playerId).foreach{ p =>
          val pos = p.position
          if (m.cells(pos._1)(pos._2).walls)
        }
      }
    }
  }
}



def generateMaze(dimension: (Int, Int), playerIds: Set[Long]): Maze = {
  val r = Random
  Maze(
    Array.ofDim(dimension._1, dimension._2),
    playerIds.map(id => Player(id,
                               (r.nextInt(dimension._1), r.nextInt(dimension._2)))))
}
