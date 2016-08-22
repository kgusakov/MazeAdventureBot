package com.maze.bot

import akka.actor.Actor
import com.maze.bot.GameRouter.WinGame
import com.maze.bot.telegram.api.{SendMessage, TelegramApiClient, User}
import com.maze.game.Directions.Direction
import com.maze.game.{Game, Generator}
import com.maze.game.Items.{Armory, Chest, Exit, Hospital}
import com.maze.game.MovementResults.{NewCell, Wall, Win}
import com.maze.game.ShootResults.{Injured, Miss}

import scala.util.Random
import scalaz.Scalaz._

object GameMaster {
  case class Move(user: User, direction: Direction)
  case class Shoot(user: User, direction: Direction)
}

class GameMaster(chatId: Int, players: Map[Int, User]) extends Actor {

  import GameMaster._

  var game: Game = _
  var startingPositionsSnapshot: Game = _

  override def preStart(): Unit = {
    game = Generator.generateGame(10, Random.nextGaussian() > 0.6, players.keySet)
    startingPositionsSnapshot = game.snapshot
    TelegramApiClient sendMessage SendMessage(chatId, nextUserPrompt)
  }

  override def receive: Receive = {
    case Move(user, direction) =>
      val message: Option[String] = game.move(user.id, direction) match {
        case None => "Sorry, not your turn".some
        case Some(NewCell(walls, items)) =>
          val wallsInfo =
            if (walls.isEmpty) "You moved to cell without walls"
            else "You moved to cell with following walls: " + walls.mkString(",")
          val itemsInfo =
            if (items.nonEmpty)
              "\nYou found following items: " + items.map {
                case Exit => "Exit"
                case Chest => "Chest"
                case Armory => "Armory"
                case Hospital => "Hospital"
              }.mkString(",")
            else ""
          (wallsInfo + itemsInfo).some
        case Some(Wall) => "Sorry dude, there is a wall".some
        case Some(Win(playerId)) =>
          sender() ! WinGame(chatId, startingPositionsSnapshot, players(playerId))
          none
      }
      for (m <- message) TelegramApiClient sendMessage SendMessage(chatId, s"$m\n\n$nextUserPrompt")
    case Shoot(user, direction) =>
      val message: Option[String] = game.shoot(user.id, direction) match {
        case None => "Sorry, not your turn".some
        case Some(Miss) =>
          s"You missed".some
        case Some(Injured(playerIds)) =>
          val injuredUsersNicks = players.filter(elem => playerIds.contains(elem._1)).values.map("@" + _.username).mkString(",")
          s"Injured players: $injuredUsersNicks".some
      }
      for (m <- message) TelegramApiClient sendMessage SendMessage(chatId, s"$m\n\n$nextUserPrompt")
  }

  def nextUser = players(game.checkCurrentPlayer.id)

  def nextUserPrompt = s"Your turn @${nextUser.username}"
}
