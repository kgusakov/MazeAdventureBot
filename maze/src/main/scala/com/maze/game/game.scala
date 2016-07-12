package com.maze.game

import com.maze.game.Directions.Direction
import com.maze.game.Items.{Exit, Item}
import com.maze.game.Results._
import com.maze.game.Walls.Wall
import com.typesafe.scalalogging.LazyLogging

import scala.util.Random
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

case class Game(playerIds: SortedSet[Int]) extends LazyLogging {

  import Game.direction2wall

  private val maze = Generator.generateMaze(10, Random.nextGaussian() > 0.2, playerIds)
  // snapshot for further usage in pictures
  private val initMazeSnapshot = Maze(maze.cells.map(_.map(_.copy())), maze.players.map(p => p.copy(position = p.position.copy())))

  private var currentPlayer: Int = playerIds.head

  def move(playerId: Int, direction: Direction): Result = {
    if (currentPlayer == playerId) {
      currentPlayer = nextPlayer
      logger.debug(s"Player ${maze.player(playerId)} is moving to ${direction}")
      val pos = maze.player(playerId).position
      if (maze.cells(pos.y)(pos.x) ?| direction) Wall
      else {
        direction match {
          case Directions.Up => pos.y -= 1
          case Directions.Down => pos.y += 1
          case Directions.Left => pos.x -= 1
          case Directions.Right => pos.x += 1
        }
        val newCell = maze.cells(pos.y)(pos.x)
        if (newCell.item contains Exit) Win(playerId)
        else NewCell(newCell.item.toSet)
      }
    }
    else NotYourTurn
  }

  def checkCurrentPlayer = currentPlayer

  def initialSnapshot = initMazeSnapshot.copy()

  private def nextPlayer: Int = {
    val playersSeq = playerIds.toSeq
    val nextIndex = playersSeq.indexOf(currentPlayer) + 1
    if (nextIndex == playersSeq.length) playersSeq.head
    else playersSeq(nextIndex)
  }

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






