import com.maze.game.Walls._
import com.maze.game.{Cell, Generator}
import org.scalatest.{FunSuite, Matchers}
import Matchers._

import scala.collection.mutable
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

    Generator.generateMaze(5, iterator.next) should be (expected)

  }
}

object DrawApp extends JFXApp {

  val mazeSize = 5

  val iterator = Stream.continually(List(true, true, true, false, false).toStream).flatten.iterator

  val cells = Generator.generateMaze(mazeSize, {iterator.next()})

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
      case Up =>
        gc.moveTo(position._1 - cellSize / 2, position._2 - cellSize / 2)
        gc.lineTo(position._1 + cellSize, position._2 - cellSize / 2)
      case Down =>
        gc.moveTo(position._1 - cellSize / 2, position._2 + cellSize / 2)
        gc.lineTo(position._1 + cellSize, position._2 + cellSize / 2)
      case Left =>
        gc.moveTo(position._1 - cellSize / 2, position._2 - cellSize / 2)
        gc.lineTo(position._1 - cellSize / 2, position._2 + cellSize)
      case Right =>
        gc.moveTo(position._1 + cellSize / 2, position._2 - cellSize / 2)
        gc.lineTo(position._1 + cellSize / 2, position._2 + cellSize)
    }
  }

}
