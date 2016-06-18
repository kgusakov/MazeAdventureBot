package com.maze.game

import com.maze.game.Directions.Direction
import com.maze.game.Items.Item
import com.maze.game.Walls.Wall

import scala.util.Random
import scala.collection.immutable.SortedSet
import scala.collection.mutable

object Walls {
  sealed trait Wall
  case object Up extends Wall
  case object Down extends Wall
  case object Left extends Wall
  case object Right extends Wall
}


object Items {
  sealed trait Item
  object Exit extends Item
  object Chest extends Item
}

object Directions {
  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction
}



case class Cell(walls: mutable.Set[Wall] = mutable.Set.empty, item: mutable.Set[Item] = mutable.Set.empty) {

  def +|=(wall: Wall): Cell = {
    walls += wall
    this
  }

  /**
    * Check if cell has input wall
    *
    * @param wall
    * @return
    */
  def ?|(wall: Wall): Boolean = {
    walls.contains(wall)
  }
}

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

object Generator {

  private def withWall(cell: Cell, wall: Wall) = {
    cell.copy(cell.walls + wall)
  }

  def generateMaze(mazeSize: Int, wallChance: => Boolean) = {
    val cells = mutable.ArraySeq.fill(mazeSize)(mutable.ArraySeq.fill(mazeSize)(Cell()))

    def buildWall(pos: (Int, Int), wall: Wall): Unit = {
      val (y,x) = pos
      wall match {
        case Walls.Down =>
          cells(y)(x) +|= Walls.Down
          cells(y + 1)(x) +|= Walls.Up
        case Walls.Up =>
          cells(y)(x) +|= Walls.Up
          cells(y - 1)(x) +|= Walls.Down
        case Walls.Right =>
          cells(y)(x) +|= Walls.Right
          cells(y)(x + 1) +|= Walls.Left
        case Walls.Left =>
          cells(y)(x) +|= Walls.Left
          cells(y)(x - 1) +|= Walls.Right
      }
    }

    cells(0) foreach (_ +|= Walls.Up)
    cells(mazeSize - 1) foreach (_ +|= Walls.Down)
    for (row <- cells) {
      row(0) +|= Walls.Left
      row(mazeSize - 1) +|= Walls.Right
    }

    val group = mutable.ArraySeq.range(0, mazeSize)

    var i = mazeSize

    for (y <- 0 until (mazeSize - 1)) {
      for (x <- 0 until (mazeSize - 1)) {
        if (group(x + 1) == group(x)) {
          if (wallChance) {
            buildWall((y, x), Walls.Right)
            if (cells(y)(x) ?| Walls.Up || cells(y)(x + 1) ?| Walls.Up) {
              group(x + 1) = i
              i = i + 1
            }
          }
        } else {
          if (wallChance) {
            buildWall((y,x), Walls.Right)
            group(x + 1) = i
            i = i + 1
          } else group(x + 1) = group(x)
        }
      }

      var f = false
      for (x <- 0 until (mazeSize - 1)) {
        if (group(x) == group(x + 1)) {
          if (wallChance) buildWall((y,x), Walls.Down)
          else f = true
        } else {
          if (f && wallChance) buildWall((y,x), Walls.Down)
          f = false
        }
      }

      if ((group(mazeSize - 2) == group(mazeSize - 1)) && f) {
        if (wallChance) buildWall((y, mazeSize - 1), Walls.Down)
      }
    }

    for (x <- 0 until (mazeSize - 1)) {
      if ((group(x) == group(x + 1)) && !cells(mazeSize - 1)(x).?|(Walls.Up) &&
        !cells(mazeSize - 1)(x + 1).?|(Walls.Up)) {

        if (wallChance) buildWall((mazeSize - 1, x), Walls.Right)
      }
    }
    cells
  }
}




