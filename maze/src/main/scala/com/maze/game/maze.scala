package com.maze.game

import com.maze.game.Items.{Chest, Exit, Item}
import com.maze.game.Walls.Wall

import scala.collection.mutable
import scala.math.Ordering

object Walls {
  sealed trait Wall extends Serializable
  case object Up extends Wall
  case object Down extends Wall
  case object Left extends Wall
  case object Right extends Wall
}

object Items {
  sealed trait Item extends Serializable
  object Exit extends Item
  object Chest extends Item
  object Hospital extends Item
  object Armory extends Item
}

case class Cell(walls: mutable.Set[Wall] = mutable.Set.empty, items: mutable.Set[Item] = mutable.Set.empty) extends Serializable {

  def hasChest = items contains Chest

  def hasExit = items contains Exit

  def has(item: Item) = items contains item

  def add(item: Item) = items += item

  def addChest() = items += Chest

  def +|=(wall: Wall): Cell = {
    walls += wall
    this
  }

  def ?|(wall: Wall): Boolean = {
    walls.contains(wall)
  }

  def removeChest() = {
    items -= Chest
    this
  }
}

case class Maze(cells: Array[Array[Cell]]) extends Serializable

case class Position(var x: Int, var y: Int) extends Serializable
case class Player(id: Int, position: Position, withChest: Boolean = false, withAmmunition: Int = 3) extends Serializable {
  private var chest = withChest

  private val maxAmmunition = withAmmunition
  private var ammunition = withAmmunition
  private var injured = false
  def hasChest = chest

  def takeChest() { chest = true }
  def dropChest() { chest = false }
  def hasAmmo = ammunition > 0
  def shoot() { if (hasAmmo) ammunition -= 1}
  def rearm() =
    ammunition = maxAmmunition

  def injure() {
    injured = true
  }

  def isInjured = injured
  def isHealthy = !injured

  def heal() {
    injured = false
  }
}

object Player extends Serializable {
  trait PlayerOrdering extends Ordering[Player] {
    def compare(x: Player, y: Player) = x.id.compare(y.id)
  }
  implicit object Player extends PlayerOrdering
}
