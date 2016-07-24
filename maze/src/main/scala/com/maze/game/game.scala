package com.maze.game

import com.maze.game.Directions.Direction
import com.maze.game.Items.{Chest, Exit, Item}
import com.maze.game.Results._
import com.maze.game.Walls.Wall
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.SortedSet

object Directions {
  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction
}

object Results {
  sealed trait Result
  case object Wall extends Result
  case class Win(playerId: Int) extends Result
  case class NewCell(items: Set[Item]) extends Result
  case object NotYourTurn extends Result
}

case class Game(maze: Maze, players: Set[Player]) extends LazyLogging {

  def move(playerId: Int, direction: Direction): Result = {
    import Game.direction2wall

    if (currentPlayer == playerId) {
      currentPlayer = nextPlayer
      logger.debug(s"Player ${player(playerId)} is moving to $direction")
      val pos = player(playerId).position
      if (maze.cells(pos.y)(pos.x) ?| direction) Wall
      else {
        direction match {
          case Directions.Up => pos.y -= 1
          case Directions.Down => pos.y += 1
          case Directions.Left => pos.x -= 1
          case Directions.Right => pos.x += 1
        }
        maze.cells(pos.y)(pos.x) match {
          case cell if (cell.item contains Exit) && player(playerId).hasChest =>
            Win(playerId)
          case cell if cell.item contains Chest =>
            player(playerId).hasChest = true
            cell removeChest()
            NewCell(cell.item.toSet + Chest)
          case cell => NewCell(cell.item.toSet)
        }
      }
    }
    else NotYourTurn
  }

  def checkCurrentPlayer = currentPlayer

  def snapshot = {
    copy(
        maze.copy(maze.cells.map(_.map(_.copy()))),
        players.map(p => p.copy(position = p.position.copy())))
  }

  private def nextPlayer: Int = {
    val playersSeq = players.map(_.id).toSeq
    val nextIndex = playersSeq.indexOf(currentPlayer) + 1
    if (nextIndex == playersSeq.length) playersSeq.head
    else playersSeq(nextIndex)
  }

  private def player(id: Int) = players.find(_.id == id).get

  private val sortedPlayers = SortedSet.empty[Player] ++ players
  private var currentPlayer: Int = sortedPlayers.map(_.id).head

}

object Game {
  import scala.language.implicitConversions

  implicit def direction2wall(direction: Direction): Wall = {
    direction match {
      case Directions.Up => Walls.Up
      case Directions.Down => Walls.Down
      case Directions.Left => Walls.Left
      case Directions.Right => Walls.Right
    }
  }
}






