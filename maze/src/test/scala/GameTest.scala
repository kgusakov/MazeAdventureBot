import com.maze.game.{MovementResults, _}
import org.scalatest._
import Matchers._
import com.maze.game.Items.{Armory, Chest, Exit, Hospital}
import com.maze.game.MovementResults.{NewCell}
import com.maze.game.ShootResults.{Injured}
import com.maze.game.Walls.{Down, Left, Right, Up}
import utils.MazeDescriptionDSL._

import OptionValues._
import utils.CustomMatchers

import scalaz.Scalaz._
import scala.util.Random

class GameTest extends FeatureSpec with GivenWhenThen with CustomMatchers {

  feature("game generation") {
    scenario("maze borders and item generation") {
      Given("size of maze and function with wall generating possibility")
      val size = 10
      def wallChance = Random.nextGaussian() > 0.2
      When("generate some mazes")
      val mazes = for (i <- 0 to 1000) yield Generator.generateCells(10, Random.nextGaussian() > 0.2)
      Then("all of them should have correct walls on borders, one chest, one exit, one hospital and one armory")
      for (cells <- mazes) {
        cells.map(_.count(_.hasChest)).sum should be (1)
        cells.map(_.count(_.hasExit)).sum should be (1)
        cells.map(_.count(_.items contains Hospital)).sum should be (1)
        cells.map(_.count(_.items contains Armory)).sum should be (1)

        cells.head.forall(_ ?| Walls.Up) should be(true)
        cells.last.forall(_ ?| Walls.Down) should be(true)
        cells.map(_.head).forall(_ ?| Walls.Left) should be(true)
        cells.map(_.last).forall(_ ?| Walls.Right) should be(true)
      }
    }
  }

  feature("hospital") {
    scenario("injured player found the hospital") {
      Given("game with 1 injured player")
      val player = p(1, 0, 0, false, 1)
      player injure()
      player.isInjured should be (true)
      val game = g(
        r(c(Up, Left)(), c(Up, Right)(Hospital)),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(player)
      When("player go to hospital")
      val moveResult = game.move(1, Directions.Right)
      Then("he should become healed")
      player.isHealthy should be (true)
      And("result should be cell with hospital")
      moveResult.value should hasOnlyItem (Hospital)
    }
  }

  feature("armory") {
    scenario("player without ammo found the armory") {
      Given("game with 1 player without ammo")
      val player = p(1, 0, 0, false, 3)
      player.shoot()
      player.shoot()
      player.shoot()
      player.hasAmmo should be (false)
      val game = g(
        r(c(Up, Left)(), c(Up, Right)(Armory)),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(player)
      When("player go to hospital")
      val moveResult = game.move(1, Directions.Right)
      Then("he should receive ammo")
      player.hasAmmo should be (true)
      And("result should be cell with hospital")
      moveResult.value should hasOnlyItem(Armory)
    }
  }

  feature ("shooting") {
    scenario("player shoot to empty left cells") {
      Given("game with 1 player, which has ammo")
      val game = g(
        r(c(Up, Left)(), c(Up, Right)()),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(p(1, 0, 0, false, 1))
      When("player shoots to empty cell")
      val shootResult = game.shoot(1, Directions.Right)
      Then("result should be miss")
      shootResult should contain (ShootResults.Miss)
    }

    scenario("player shoots to another player without ammo") {
      Given("game with 2 players, which has no ammo")
      val game = g(
        r(c(Up, Left)(), c(Up, Right)()),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(p(1, 0, 0, false, 0), p(2, 1, 0, false, 1))
      When("player without ammo shoots to another player")
      val shootResult = game.shoot(1, Directions.Right)
      Then("result should be miss")
      shootResult should contain (ShootResults.Miss)
    }

    scenario("injured player shoots to another player") {
      Given("game with 2 injured players")
      val game = g(
        r(c(Up, Left)(), c(Up, Right)()),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(p(1, 0, 0, false, 3), p(2, 1, 0, false, 3))
      game.players.find(_.id == 1).foreach(_.injure())
      When("injured player shoots to another player")
      val shootResult = game.shoot(1, Directions.Right)
      Then("result should be miss")
      shootResult should contain (ShootResults.Miss)
    }

    scenario("player shoots to another player through wall") {
      Given("game with 2 players")
      val game = g(
        r(c(Up, Left)(), c(Up, Right)(), c(Up, Right, Left)()),
        r(c(Left)(), c()(), c(Right)()),
        r(c(Left, Down)(), c(Left, Down)(), c(Right, Down)(Exit))
      )(p(1, 0, 0, false, 1), p(2, 2, 0, false, 1))
      When("player shoots to another player through wall")
      val shootResult = game.shoot(1, Directions.Right)
      Then("result should be miss")
      shootResult should contain (ShootResults.Miss)
    }

    scenario("shooting in all 4 directions") {
      Given("game 2x2")
      val game = g(
        r(c(Up, Left)(),  c(Up, Right)()),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )_

      When("left player shoots to right player")
      val shootResult1 = game(Seq(p(1, 0, 0, false, 1), p(2, 1, 0, false, 1))).shoot(1, Directions.Right)
      Then("result should be wound of right player")
      shootResult1 should contain (Injured(Set(2)))

      When("top player shoots to bottom player")
      val shootResult2 = game(Seq(p(1, 0, 0, false, 1), p(2, 0, 1, false, 1))).shoot(1, Directions.Down)
      Then("result should be wound of bottom player")
      shootResult2 should contain (Injured(Set(2)))

      When("right player shoots to left player")
      val shootResult3 = game(Seq(p(1, 1, 0, false, 1), p(2, 0, 0, false, 1))).shoot(1, Directions.Left)
      Then("result should be wound of bottom player")
      shootResult3 should contain (Injured(Set(2)))

      When("bottom player shoots to top player 1")
      val shootResult4 = game(Seq(p(1, 0, 1, false, 1), p(2, 0, 0, false, 1))).shoot(1, Directions.Up)
      Then("result should be wound of top player")
      shootResult4 should contain (Injured(Set(2)))
    }

    scenario("player with ammo shoots to another 2 players (in one cell)") {
      Given("game with 3 players, which has ammo")
      val game = g(
        r(c(Up, Left)(), c(Up)(), c(Up, Right)()),
        r(c(Left)(), c()(), c(Right)()),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(p(1, 0, 0, false, 1), p(2, 2, 0, false, 1), p(3, 2, 0, false, 1))
      When("player shoots to another players")
      val shootResult = game.shoot(1, Directions.Right)
      Then("result should be wound of 2 players")
      shootResult should contain (Injured(Set(2,3)))
      all (game.players.filter(Set(2,3) contains _.id).map(_.isInjured)) should be (true)
    }

    scenario("wound of player with chest") {
      Given("game with 2 players, one has chest")
      val game = g(
        r(c(Up, Left)(), c(Up, Right)()),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(p(1, 0, 0, false, 1), p(2, 1, 0, true, 1))
      When("player shoots to player with chest")
      val shootResult = game.shoot(1, Directions.Right)
      Then("result should be wound of 1 player")
      shootResult should contain (Injured(Set(2)))
      And("Injured player should drop chest")
      game.maze.cells(0)(1).items.contains(Chest) should be (true)
      game.players.find(_.id == 2).get.hasChest should be (false)

    }
  }

  feature ("player movement") {

    scenario("player tries to move in not his turn") {
      Given("game with 2 players")
      val game = g(
        r(c(Up, Left)(), c(Up, Right)()),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(p(1, 0, 0, false), p(2, 1, 1, false))
      When("player 2 trying to move before player 1")
      val movementResult = game.move(2, Directions.Right)
      Then("result should be not your turn")
      movementResult should not be defined
    }

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
      movementResult should contain (NewCell(Set(Up, Right), Set.empty))
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
      movementResult should contain (MovementResults.Wall)
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
      movementResult.value should hasOnlyItem(Chest)
      And("player 1 should receive chest in his pocket")
      players(0).hasChest should be (true)
      And("player 2 should have no chest")
      players(1).hasChest should be (false)
      And("chest should disappear from cell")
      game.maze.cells(1)(1).items should be (Set.empty)
    }

    scenario("taking the chest by injured player") {
      Given("game with 2 injured players without chest")
      val players = Array(p(1, 0, 1, false), p(2, 0, 0, false))
      players.foreach(_.injure())
      val game = g(
        r(c(Up, Left)(), c(Up, Right)()),
        r(c(Left)(), c(Right)(Chest)),
        r(c(Left, Down)(), c(Right, Down)(Exit))
      )(players: _*)
      When("player 1 go to cell with chest")
      val movementResult = game.move(1, Directions.Right)
      Then("result should be new cell with chest")
      movementResult.value should hasOnlyItem(Chest)
      And("player 1 should not receive chest in his pocket")
      players(0).hasChest should be (false)
      And("chest should not disappear from cell")
      game.maze.cells(1)(1).items should be (Set(Chest))
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
      movementResult should contain (MovementResults.Win(1))
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
      movementResult.value should hasOnlyItem(Exit)
    }
  }
}
