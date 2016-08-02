package utils

import com.maze.game.Items.Item
import com.maze.game._
import com.maze.game.Walls.Wall

import scala.collection.mutable

/**
  * Created by kgusakov on 16.07.16.
  */
object MazeDescriptionDSL {

  def c(walls: Wall*)(items: Item*): Cell = {
    Cell(mutable.Set.empty ++ walls, mutable.Set.empty ++ items)
  }

  def r(cells: Cell*): Array[Cell] = Array(cells: _*)

  def p(id: Int, x: Int, y: Int, hasChest: Boolean = false, ammunition: Int = 0) = Player(id, Position(x, y), hasChest, ammunition)

  def g(rows: Array[Cell]*)(players: Player*) = Game(Maze(Array(rows: _*)), Set(players: _*))
}
