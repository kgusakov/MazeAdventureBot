import com.maze.game._
import org.scalatest.{FunSuite, Matchers}

import scala.util.{Left, Right}
import Matchers._

import scala.collection.mutable
import scala.collection.immutable.SortedSet
import scala.util.Random
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.layout.{HBox, Pane}
import scalafx.scene.paint.Color._

class MazeTest extends FunSuite {
  val expected = mutable.ArraySeq(mutable.ArraySeq(Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Left),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Right, com.maze.game.Walls.Down),mutable.Set())), mutable.ArraySeq(Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Right, com.maze.game.Walls.Up),mutable.Set())), mutable.ArraySeq(Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Right, com.maze.game.Walls.Left),mutable.Set())), mutable.ArraySeq(Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Right, com.maze.game.Walls.Left),mutable.Set())), mutable.ArraySeq(Cell(mutable.Set(com.maze.game.Walls.Down, com.maze.game.Walls.Left, com.maze.game.Walls.Up),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Down, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Down, com.maze.game.Walls.Left),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Down, com.maze.game.Walls.Right),mutable.Set())))

  test ("maze generation test") {
    val iterator = Stream.continually(List(true, true, true, false, false).toStream).flatten.iterator

    Generator.generateCells(5, iterator.next) should be (expected)
  }

  test ("maze generic walls test") {
    for (i <- 0 to 1000) {
      val cells = Generator.generateCells(10, Random.nextGaussian() > 0.2)
      cells.head.forall(_ ?| Walls.Up) should be (true)
      cells.last.forall(_ ?| Walls.Down) should be (true)
      cells.map(_.head).forall(_ ?| Walls.Left) should be (true)
      cells.map(_.last).forall(_ ?| Walls.Right) should be (true)
    }
  }

  test ("game movement") {
    for (i <- 0 to 1000) {
      val game = Game(SortedSet(1))
      game.move(1, Directions.Up) match {
        case Left(s) => game.move(1, Directions.Up) should be ('left)
        case Right(cell) => game.move(1, Directions.Down) should be ('right)
      }
      game.move(1, Directions.Right) match {
        case Left(s) => game.move(1, Directions.Right) should be ('left)
        case Right(cell) => game.move(1, Directions.Left) should be ('right)
      }
    }
  }
}

object DrawApp extends JFXApp {

  val mazeSize = 15

  val iterator = Stream.continually(List(true, true, true, false, false).toStream).flatten.iterator

  val cells = Generator.generateCells(mazeSize, {Random.nextGaussian() > 0.2})

  val canvasSize = 300
  val canvas = new Canvas(canvasSize, canvasSize)
  val cellSize = canvasSize/mazeSize
  val gc = canvas.graphicsContext2D
  gc.beginPath()
  for (y <- cells.indices) {
    for (x <- cells.indices) {
      drawCell(gc, (x * cellSize + cellSize / 2, y * cellSize + cellSize / 2), cells(y)(x), cellSize)
    }
  }
  gc.strokePath()

  stage = new PrimaryStage {
    title = "ScalaFX Hello World"
    scene = new Scene {
      fill = WhiteSmoke
      content = new Pane {
        padding = Insets(20,20,20,20)
        children = Seq(
          canvas
        )
      }
    }
  }

  def drawCell(gc: GraphicsContext, position: (Double, Double), cell: Cell, cellSize: Double) {
    cell.walls.foreach {
      case Walls.Up =>
        gc.moveTo(position._1 - cellSize / 2, position._2 - cellSize / 2)
        gc.lineTo(position._1 + cellSize, position._2 - cellSize / 2)
      case Walls.Down =>
        gc.moveTo(position._1 - cellSize / 2, position._2 + cellSize / 2)
        gc.lineTo(position._1 + cellSize, position._2 + cellSize / 2)
      case Walls.Left =>
        gc.moveTo(position._1 - cellSize / 2, position._2 - cellSize / 2)
        gc.lineTo(position._1 - cellSize / 2, position._2 + cellSize)
      case Walls.Right =>
        gc.moveTo(position._1 + cellSize / 2, position._2 - cellSize / 2)
        gc.lineTo(position._1 + cellSize / 2, position._2 + cellSize)
    }
  }

}
