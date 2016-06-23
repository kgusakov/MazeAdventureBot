package com.maze.game

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage
import java.io.OutputStream
import java.util.UUID

import com.maze.game.Directions.Direction
import com.maze.game.Items.{Exit, Item}
import com.maze.game.Walls.Wall
import com.typesafe.scalalogging.LazyLogging

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

case class Maze(cells: Array[Array[Cell]], players: Set[Player]) {
  def player(id: Int) = players.find(_.id == id).get
}

case class Position(var x: Int, var y: Int)
case class Player(id: Int, position: Position)

case class Game(playerIds: SortedSet[Int]) extends LazyLogging {

  import Game.direction2wall

  private val maze = Generator.generateMaze(10, Random.nextGaussian() > 0.2, playerIds)

  private var currentPlayer: Int = playerIds.head

  def move(playerId: Int, direction: Direction): Either[String, Option[Cell]] = {
    if (currentPlayer == playerId) {
      currentPlayer = nextPlayer
      logger.debug(s"Player ${maze.player(playerId)} is moving to ${direction}")
      val pos = maze.player(playerId).position
      if (maze.cells(pos.y)(pos.x) ?| direction) Right(None)
      else {
        direction match {
          case Directions.Up => pos.y -= 1
          case Directions.Down => pos.y += 1
          case Directions.Left => pos.x -= 1
          case Directions.Right => pos.x += 1
        }
        Right(Some(maze.cells(pos.y)(pos.x)))
      }
    }
    else Left("Not you turn")
  }

  def checkCurrentPlayer = currentPlayer

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

object Generator {

  def generateMaze(mazeSize: Int, wallChance: => Boolean, playerIds: SortedSet[Int]): Maze = {
    val r = Random
    Maze(
      Generator.generateCells(mazeSize, wallChance),
      playerIds.map(id => Player(id,
        Position(r.nextInt(mazeSize), r.nextInt(mazeSize)))))
  }

  def generateCells(mazeSize: Int, wallChance: => Boolean) = {
    val cells = Array.fill(mazeSize)(Array.fill(mazeSize)(Cell()))

    def buildWall(pos: (Int, Int), wall: Wall): Unit = {
      val (y,x) = pos
      wall match {
        case Walls.Down =>
          cells(y)(x) +|= Walls.Down
          cells(y + 1)(x) +|= Walls.Up
        case Walls.Right =>
          cells(y)(x) +|= Walls.Right
          cells(y)(x + 1) +|= Walls.Left
      }
    }

    cells.head foreach (_ +|= Walls.Up)
    cells(mazeSize - 1) foreach (_ +|= Walls.Down)
    for (row <- cells) {
      row.head +|= Walls.Left
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

    val edges = cells.head ++ cells.last ++ cells.map(_.head) ++ cells.map(_.last)
    edges(Random.nextInt(edges.length)).item.add(Exit)
    cells
  }
}

object Drawer extends LazyLogging {

  def drawMaze(maze: Maze, out: OutputStream) = {
    val cells = maze.cells
    val mazeSize = maze.cells.length

    val canvasSize = 300

    val canvas = new BufferedImage(canvasSize, canvasSize, BufferedImage.TYPE_INT_RGB)
    val gc = canvas.createGraphics()
    gc.setColor(Color.WHITE)

    val margin = 10
    val cellSize = (canvasSize - 2 * margin) / mazeSize
    for (y <- cells.indices) {
      for (x <- cells.indices) {
        drawCell(gc, (margin + x * cellSize + cellSize / 2, margin + y * cellSize + cellSize / 2), cells(y)(x), cellSize)
      }
    }
    for (player <- maze.players) {
      drawPlayer(gc, (margin + player.position.x * cellSize + cellSize / 2, margin + player.position.y * cellSize + cellSize / 2), cellSize)
    }
    gc.dispose()
    javax.imageio.ImageIO.write(canvas, "png", out)
    logger.debug("Maze drawed")
  }

  def drawPlayer(gc: Graphics2D, position: (Int, Int), cellSize: Int): Unit = {
    gc.setColor(Color.GREEN)
    gc.fillRect(position._1 - cellSize / 4, position._2 - cellSize / 4, cellSize / 2, cellSize / 2)
  }

  def drawCell(gc: Graphics2D, position: (Int, Int), cell: Cell, cellSize: Int) {
    if (cell.item.contains(Items.Exit)) {
      logger.debug("Exit drawing")
      gc.setColor(Color.RED)
      gc.fillRect(position._1 - cellSize / 2, position._2 - cellSize / 2, cellSize, cellSize)
    }
    gc.setColor(Color.WHITE)
    cell.walls.foreach {
      case Walls.Up =>
        gc.drawLine(position._1 - cellSize / 2, position._2 - cellSize / 2, position._1 + cellSize / 2, position._2 - cellSize / 2)
      case Walls.Down =>
        gc.drawLine(position._1 - cellSize / 2, position._2 + cellSize / 2, position._1 + cellSize / 2, position._2 + cellSize / 2)
      case Walls.Left =>
        gc.drawLine(position._1 - cellSize / 2, position._2 - cellSize / 2, position._1 - cellSize / 2, position._2 + cellSize / 2)
      case Walls.Right =>
        gc.drawLine(position._1 + cellSize / 2, position._2 - cellSize / 2, position._1 + cellSize / 2, position._2 + cellSize / 2)
    }
  }
}




