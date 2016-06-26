package com.maze.game

import com.maze.game.Items.Item
import com.maze.game.Walls.Wall

import scala.collection.mutable

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
}

case class Maze(cells: Array[Array[Cell]], players: Set[Player]) {
  def player(id: Int) = players.find(_.id == id).get
}

case class Position(var x: Int, var y: Int)
case class Player(id: Int, position: Position)
