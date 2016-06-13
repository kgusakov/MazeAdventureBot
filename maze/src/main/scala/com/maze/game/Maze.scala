package com.maze.game

import com.maze.game.Directions.Direction
import com.maze.game.Items.Item
import com.maze.game.Walls.Wall

import scala.util.Random
import scala.collection.immutable.SortedSet

object Walls {
  sealed trait Wall
  object Up extends Wall
  object Down extends Wall
  object Left extends Wall
  object Right extends Wall
}


object Items {
  sealed trait Item
  object Exit extends Item
  object Chest extends Item
}

object Directions {
  sealed trait Direction
  object Up extends Direction
  object Down extends Direction
  object Left extends Direction
  object Right extends Direction
}



case class Cell(walls: Set[Wall] = Set.empty, item: Set[Item] = Set.empty)

case class Maze(cells: Array[Array[Cell]], players: Set[Player])

case class Player(id: Int, position: (Int, Int))

case class Game(playerIds: SortedSet[Int]) {

  private val rand = Random

  private val maze = generateMaze((10, 10), playerIds)

  private var currentPlayer: Int = playerIds.head

  def move(playerId: Int, direction: Direction): Either[String, Option[Cell]] = {
    if (currentPlayer == playerId) {
      currentPlayer = nextPlayer
      if (rand.nextBoolean) Right(None)
      else Right(Some(new Cell))
    }
    else Left("Not you turn")
  }

  private def nextPlayer: Int = {
    val playersSeq = playerIds.toSeq
    val nextIndex = playersSeq.indexOf(currentPlayer) + 1
    if (nextIndex == playersSeq.length) playersSeq(0)
    else playersSeq(nextIndex)
  }

  private def generateMaze(dimension: (Int, Int), playerIds: SortedSet[Int]): Maze = {
    val r = Random
    Maze(
      Array.ofDim(dimension._1, dimension._2),
      playerIds.map(id => Player(id,
        (r.nextInt(dimension._1), r.nextInt(dimension._2)))))
  }
}




