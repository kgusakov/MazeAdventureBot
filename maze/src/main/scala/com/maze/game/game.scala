package com.maze.game

import com.maze.game.Directions.Direction
import com.maze.game.ShootResults.{GameOver, Injured, Miss, ShootResult}
import com.maze.game.Items._
import com.maze.game.MovementResults._
import com.maze.game.Walls.Wall
import com.typesafe.scalalogging.LazyLogging

import scalaz.Scalaz._
import scala.collection.immutable.SortedSet

object Directions {
  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction
}

object MovementResults {
  sealed trait MovementResult
  case object Wall extends MovementResult
  case class Win(playerId: Int) extends MovementResult
  case class NewCell(items: Set[Item]) extends MovementResult
}

object ShootResults {
  sealed trait ShootResult
  case class Injured(playerId: Set[Int]) extends ShootResult
  object GameOver extends ShootResult
  case object Miss extends ShootResult
}

case class Game(maze: Maze, players: Set[Player]) extends LazyLogging {

  def shoot(playerId: Int, direction: Direction): Option[ShootResult] = {
    if (currentPlayer.id == playerId) {
      val curPlayer = currentPlayer
      logger.debug(s"Player ${player(playerId)} is shooting to $direction")
      currentPlayer = nextPlayer
      if (curPlayer.hasAmmo && curPlayer.isHealthy) {
        curPlayer.shoot()
        val bulletStoppedPosition = scanObstacles(players - curPlayer, maze.cells, curPlayer.position, direction)
        bulletStoppedPosition match {
          case pos: Position if (players - curPlayer).exists(p => p.position == pos) =>
            val injuredPlayers = (players - curPlayer).filter(p => p.position == pos)
            injuredPlayers.foreach(_.injure())
            injuredPlayers.find(_.hasChest) match {
              case Some(p) =>
                p.dropChest()
                maze.cells(pos.y)(pos.x).addChest()
              case None =>
            }
            if (players.forall(_.isInjured)) GameOver
            else Injured(injuredPlayers.map(_.id))
          case _ => Miss
        }
      } else {
        Miss
      }
    }.some
    else none
  }

  def scanObstacles(players: Set[Player], cells: Array[Array[Cell]], position: Position, direction: Direction): Position = {
    import Game._
    val currentCell = cells(position.y)(position.x)
    if (currentCell.?|(direction) || players.exists(_.position == position)) position
    else {
      scanObstacles(players, cells, step(position, direction), direction)
    }
  }

  def move(playerId: Int, direction: Direction): Option[MovementResult] = {
    import Game.direction2wall

    if (currentPlayer.id == playerId) {
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

        val cell = maze.cells(pos.y)(pos.x)
        val preprocessChain: Seq[PartialFunction[Cell, Unit]] = Seq (
          {case c if c has Hospital => player(playerId).heal()},
          {case c if c has Armory => player(playerId).rearm()}
        )
        preprocessChain.filter(_.isDefinedAt(cell)).foreach(_(cell))

        cell match {
          case c if (c has Exit) && (player(playerId) hasChest) =>
            Win(playerId)
          case c if (c has Chest) && (player(playerId) isHealthy) =>
            player(playerId).takeChest()
            cell removeChest()
            NewCell(c.items.toSet + Chest)
          case c => NewCell(c.items.toSet)
        }
      }
    }.some
    else none
  }

  def checkCurrentPlayer = currentPlayer

  def snapshot = {
    copy(
        maze.copy(maze.cells.map(_.map(_.copy()))),
        players.map(p => p.copy(position = p.position.copy())))
  }

  private def nextPlayer = {
    val nextIndex = sortedPlayers.indexOf(currentPlayer) + 1
    if (nextIndex == players.size) sortedPlayers.head
    else sortedPlayers(nextIndex)
  }

  private def player(id: Int) = players.find(_.id == id).get

  private val sortedPlayers = players.toSeq.sortBy(_.id)
  private var currentPlayer = sortedPlayers.head

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

  def step(position: Position, direction: Direction): Position = {
    direction match {
      case Directions.Left => position.copy(x = position.x - 1)
      case Directions.Right => position.copy(x = position.x + 1)
      case Directions.Down => position.copy(y = position.y + 1)
      case Directions.Up => position.copy(y = position.y - 1)
    }
  }
}






