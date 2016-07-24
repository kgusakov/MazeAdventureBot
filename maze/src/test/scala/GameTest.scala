import com.maze.game.{Results, _}
import org.scalatest._
import Matchers._
import com.maze.game.Items.{Chest, Exit}
import com.maze.game.Walls.{Down, Left, Right, Up}
import utils.MazeDescriptionDSL._

import scala.util.Random

class GameTest extends FeatureSpec with GivenWhenThen {

  feature("game generation") {
    scenario("maze borders generation") {
      Given("size of maze and function with wall generating possibility")
      val size = 10
      def wallChance = Random.nextGaussian() > 0.2
      When("generate some mazes")
      val mazes = for (i <- 0 to 1000) yield Generator.generateCells(10, Random.nextGaussian() > 0.2)
      Then("all of them should have correct walls on borders")
      for (cells <- mazes) {
        val cells = Generator.generateCells(10, Random.nextGaussian() > 0.2)
        cells.head.forall(_ ?| Walls.Up) should be(true)
        cells.last.forall(_ ?| Walls.Down) should be(true)
        cells.map(_.head).forall(_ ?| Walls.Left) should be(true)
        cells.map(_.last).forall(_ ?| Walls.Right) should be(true)
      }
    }
  }

  feature ("player movement") {

    scenario("movement to empty cell") {
      Given("game with 1 player")
      val game = g(
        r(c(Up, Left)(), c(Up, Right)()),
        r(c(Left)(), c(Right)()),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(p(1, 0, 0, false))
      When("move to empty cell")
      val movementResult = game.move(1, Directions.Right)
      Then("result should be new empty cell")
      movementResult should be(Results.NewCell(Set.empty))
    }

    scenario("movement to wall") {
      Given("game with 1 player near the wall")
      val game = g(
        r(c(Up, Left)(), c(Up, Right)()),
        r(c(Left)(), c(Right)()),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(p(1, 0, 0, false))
      When("player moving to wall")
      val movementResult = game.move(1, Directions.Left)
      Then("result should be the wall")
      movementResult should be(Results.Wall)
    }

    scenario("taking the chest") {
      Given("game with 2 players without chest")
      val players = Array(p(1, 0, 1, false), p(2, 0, 0, false))
      val game = g(
        r(c(Up, Left)(), c(Up, Right)()),
        r(c(Left)(), c(Right)(Chest)),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(players: _*)
      When("player 1 go to cell with chest")
      val movementResult = game.move(1, Directions.Right)
      Then("result should be new cell with chest")
      movementResult should be(Results.NewCell(Set(Chest)))
      And("player 1 should receive chest in his pocket")
      players(0).hasChest should be (true)
      And("player 2 should have no chest")
      players(1).hasChest should be (false)
      And("chest should disappear from cell")
      game.maze.cells(1)(1).item should be (Set.empty)
    }

    scenario("win after moving to exit with chest") {
      Given("game with two players where one player already has chest")
      val game = g(
        r(c(Up, Left)(), c(Up)(), c(Up, Right)()),
        r(c(Left)(), c()(), c(Right)()),
        r(c(Left, Down)(), c(Down)(), c(Right, Down)(Exit))
      )(p(1, 1, 2, true), p(2, 0, 0, false))
      When("player with chest moving to exit")
      val movementResult = game.move(1, Directions.Right)
      Then("result should be the win of this player")
      movementResult should be(Results.Win(1))
    }

    scenario("no win after moving to exit without chest") {
      Given("game with two players where one player already has chest")
      val game = g(
        r(c(Up, Left)(Chest), c(Up)(), c(Up, Right)()),
        r(c(Left)(), c()(), c(Right)()),
        r(c(Left, Down)(), c(Down)(), c(Right, Down)(Exit))
      )(p(1, 1, 2, false), p(2, 0, 0, true))
      When("player without chest moving to exit")
      val movementResult = game.move(1, Directions.Right)
      Then("result should be the cell with exit")
      movementResult should be (Results.NewCell(Set(Exit)))
    }
  }
}
