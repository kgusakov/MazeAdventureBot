import com.maze.game.{Results, _}
import org.scalatest.{FunSuite, Matchers}

import Matchers._
import com.maze.game.Items.{Chest, Exit}
import com.maze.game.Walls.{Down, Left, Right, Up}
import utils.MazeDescriptionDSL._

import scala.util.Random

class MazeTest extends FunSuite {

  test ("maze generic walls test") {
    for (i <- 0 to 1000) {
      val cells = Generator.generateCells(10, Random.nextGaussian() > 0.2)
      cells.head.forall(_ ?| Walls.Up) should be (true)
      cells.last.forall(_ ?| Walls.Down) should be (true)
      cells.map(_.head).forall(_ ?| Walls.Left) should be (true)
      cells.map(_.last).forall(_ ?| Walls.Right) should be (true)
    }
  }

  test ("check movement to empty cell") {
    val game = g (
      r ( c(Up, Left)(),   c(Up, Right)() ),
      r ( c(Left)(),       c(Right)()),
      r ( c(Left, Down)(), c(Right, Down)(Exit))
    )(p(1, 0, 0))
    game.move(1, Directions.Right) should be (Results.NewCell(Set.empty))
  }

  test ("check movement to wall") {
    val game = g (
      r ( c(Up, Left)(),   c(Up, Right)() ),
      r ( c(Left)(),       c(Right)()),
      r ( c(Left, Down)(), c(Right, Down)(Exit))
    )(p(1, 0, 0))
    game.move(1, Directions.Left) should be (Results.Wall)
  }

  test ("check winner") {
    val game = g (
      r ( c(Up, Left)(Chest), c(Up)(), c(Up, Right)() ),
      r ( c(Left)(),          c()(),   c(Right)()),
      r ( c(Left, Down)(),    c(Down)(),   c(Right, Down)(Exit))
    )(p(1, 1, 2))
    game.move(1, Directions.Right) should be (Results.Win(1))
  }

}
