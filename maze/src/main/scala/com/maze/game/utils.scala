package com.maze.game

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage
import java.io.OutputStream

import com.maze.game.Items._
import com.maze.game.Walls.Wall
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.util.Random

object Generator {

  def generateGame(mazeSize: Int, wallChance: => Boolean, playerIds: Set[Int]): Game = {
    val r = Random
    Game(
      Maze(Generator.generateCells(mazeSize, wallChance)),
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
    edges(Random.nextInt(edges.length)).items.add(Exit)

    placeToRandomEmptyCell(cells, Chest)
    placeToRandomEmptyCell(cells, Hospital)
    placeToRandomEmptyCell(cells, Armory)

    cells
  }

  private def placeToRandomEmptyCell(cells: Array[Array[Cell]], item: Item): Unit = {
    val emptyCells = cells.flatMap(_.filter(_.items isEmpty))
    emptyCells(Random.nextInt(emptyCells.length)) add item
  }
}

object Drawer extends LazyLogging {

  def drawGame(game: Game, out: OutputStream) = {
    val cells = game.maze.cells
    val mazeSize = game.maze.cells.length

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
    for (player <- game.players) {
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
    if (cell.items.contains(Items.Exit)) {
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