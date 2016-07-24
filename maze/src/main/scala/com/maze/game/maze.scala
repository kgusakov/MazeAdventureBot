package com.maze.game

import com.maze.game.Items.{Chest, Item}
import com.maze.game.Walls.Wall

import scala.collection.mutable
import scala.math.Ordering

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

case class Cell(walls: mutable.Set[Wall] = mutable.Set.empty, item: mutable.Set[Item] = mutable.Set.empty) {

  def +|=(wall: Wall): Cell = {
    walls += wall
    this
  }

  def ?|(wall: Wall): Boolean = {
    walls.contains(wall)
  }

  def removeChest() = {
    item -= Chest
    this
  }
}

case class Maze(cells: Array[Array[Cell]])

case class Position(var x: Int, var y: Int)
case class Player(id: Int, position: Position, var hasChest: Boolean = false)

object Player {
  trait PlayerOrdering extends Ordering[Player] {
    def compare(x: Player, y: Player) = x.id.compare(y.id)
  }
  implicit object Player extends PlayerOrdering
}
