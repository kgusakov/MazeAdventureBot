import java.io.{File, FileOutputStream}
import java.util.UUID
import javax.imageio.ImageIO

import com.maze.game.{Items, _}
import org.scalatest.{FunSuite, Matchers}

import scala.util.{Left, Right}
import Matchers._

import scala.collection.mutable
import scala.collection.immutable.SortedSet
import scala.util.Random

class MazeTest extends FunSuite {
  val expected = mutable.ArraySeq(mutable.ArraySeq(Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Left),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Right, com.maze.game.Walls.Down),mutable.Set())), mutable.ArraySeq(Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Right, com.maze.game.Walls.Up),mutable.Set())), mutable.ArraySeq(Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Right, com.maze.game.Walls.Left),mutable.Set())), mutable.ArraySeq(Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Up, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Left, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Right, com.maze.game.Walls.Left),mutable.Set())), mutable.ArraySeq(Cell(mutable.Set(com.maze.game.Walls.Down, com.maze.game.Walls.Left, com.maze.game.Walls.Up),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Down, com.maze.game.Walls.Right),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Down, com.maze.game.Walls.Left),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Down),mutable.Set()), Cell(mutable.Set(com.maze.game.Walls.Down, com.maze.game.Walls.Right),mutable.Set())))

  ignore ("maze generation test") {
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

  test ("check drawner") {
    val out = new FileOutputStream("/tmp/maze/maze.png")
    Drawer.drawMaze(Generator.generateMaze(10, Random.nextGaussian() > 0.2, SortedSet(1,2)), out)
  }




}
