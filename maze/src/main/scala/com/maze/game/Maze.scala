package com.maze.game

import com.maze.game.Directions.Direction
import com.maze.game.Items.Item
import com.maze.game.Walls.Wall

import scala.util.Random
import scala.collection.immutable.SortedSet
import scala.collection.mutable

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

object Generator {

  private def withWall(cell: Cell, wall: Wall) = {
    cell.copy(cell.walls + wall)
  }

  def generateMaze(mazeSize: Int) = {
    def wallChance = Random.nextGaussian > 0.6
    val cells = mutable.ArraySeq.fill(mazeSize)(mutable.ArraySeq.fill(mazeSize)(Cell()))

    cells(0) = cells(0) map (c => c.copy(c.walls + Walls.Up))
    cells(mazeSize - 1) = cells(mazeSize - 1) map (c => c.copy(c.walls + Walls.Down))
    for (row <- cells) {
      row(0) = row(0).copy(row(0).walls + Walls.Left)
      row(mazeSize - 1) = row(mazeSize - 1).copy(row(mazeSize - 1).walls + Walls.Right)
    }

    val group = mutable.ArraySeq.fill[Int](mazeSize)(0)

    var i = mazeSize

    for (j <- 0 until mazeSize) group(j) = j

    for (y <- 0 until (mazeSize - 1)) {
      for (x <- 0 until (mazeSize - 1)) {
        if (group(x + 1) == group(x)) {
          if (wallChance) {
            cells(y)(x) = cells(y)(x).copy(cells(y)(x).walls + Walls.Right)
            cells(y)(x + 1) = cells(y)(x + 1).copy(cells(y)(x + 1).walls + Walls.Left)
            if (cells(y)(x).walls.contains(Walls.Up) || cells(y)(x + 1).walls.contains(Walls.Up)) {
              group(x + 1) = i
              i = i + 1
            }
          }
        } else {
          if (wallChance) {
            cells(y)(x) = withWall(cells(y)(x), Walls.Right)
            cells(y)(x + 1) = withWall(cells(y)(x + 1), Walls.Left)
            group(x + 1) = i
            i = i + 1
          }
          group(x + 1) = group(x)
        }
      }

      var f = false
      for (x <- 0 until (mazeSize - 1)) {
        if (group(x) == group(x + 1)) {
          if (wallChance) {
            cells(y)(x) = withWall(cells(y)(x), Walls.Down)
            cells(y + 1)(x) = withWall(cells(y + 1)(x), Walls.Up)
          } else f = true
        } else {
          if (f) {
            if (wallChance) {
              cells(y)(x) = withWall(cells(y)(x), Walls.Down)
              cells(y + 1)(x) = withWall(cells(y + 1)(x), Walls.Up)
            }
          }
          f = false
        }
      }

      if ((group(mazeSize - 2) == group(mazeSize - 1)) && f) {
        if (wallChance) {
          cells(y)(mazeSize - 1) = withWall(cells(y)(mazeSize - 1), Walls.Down)
          cells(y + 1)(mazeSize - 1) = withWall(cells(y + 1)(mazeSize - 1), Walls.Up)
        }
      }
    }

    for (x <- 0 until (mazeSize - 1)) {
      if ((group(x) == group(x + 1)) &&
        !cells(mazeSize - 1)(x).walls.contains(Walls.Up) &&
        !cells(mazeSize - 1)(x + 1).walls.contains(Walls.Up)) {

        if (wallChance) {
          cells(mazeSize - 1)(x) = withWall(cells(mazeSize - 1)(x), Walls.Right)
          cells(mazeSize - 1)(x + 1) = withWall(cells(mazeSize - 1)(x + 1), Walls.Left)
        }
      }
    }
    cells
  }
}




