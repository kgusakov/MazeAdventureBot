import com.maze.game.Walls._
import com.maze.game.{Cell, Generator}
import sun.font.GraphicComponent

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.{HBox, Pane}
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.text.Text
import scalafx.scene.paint.Color._

/**
  * Created by kgusakov on 18.06.16.
  */
object MazeTest extends JFXApp {

  val mazeSize = 20
  val cells = Generator.generateMaze(mazeSize)

  val canvasSize = 500
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
