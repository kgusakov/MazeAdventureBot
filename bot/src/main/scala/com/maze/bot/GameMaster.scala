package com.maze.bot

import com.maze.bot.GameMaster.WinGame
import com.maze.bot.telegram.api._
import com.maze.game.Directions.Direction
import com.maze.game.{Game, Generator}
import com.maze.game.Items.{Armory, Chest, Exit, Hospital}
import com.maze.game.MovementResults.{NewCell, Wall, Win}
import com.maze.game.ShootResults.{Injured, Miss}

import scala.util.Random
import scalaz.Scalaz._

object GameMaster {
  case class WinGame(snapshot: Game, winner: User)
}

case class GameMaster(chatId: Int, players: Map[Int, User]) {

  var game: Game = _
  var startingPositionsSnapshot: Game = _

  def start: SendMessage = {
    game = Generator.generateGame(10, Random.nextGaussian() > 0.4, players.keySet)
    startingPositionsSnapshot = game.snapshot
    promptNextUser()
  }

  def move(user: User, direction: Direction): Either[WinGame, SendMessage] = {
    val moveResult = game.move(user.id, direction) match {
      case None => Right("Sorry, not your turn")
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
        Right(wallsInfo + itemsInfo)
      case Some(Wall) => Right("Sorry dude, there is a wall")
      case Some(Win(playerId)) =>
        Left(WinGame(startingPositionsSnapshot, players(playerId)))
    }
    moveResult.right.map(message => promptNextUser(message.some))
  }

  def shoot(user: User, direction: Direction): SendMessage = {
    val shootResult = game.shoot(user.id, direction) match {
      case None => "Sorry, not your turn"
      case Some(Miss) =>
        s"You missed"
      case Some(Injured(playerIds)) =>
        val injuredUsersNicks = players.filter(elem => playerIds.contains(elem._1)).values.map("@" + _.username).mkString(",")
        s"Injured players: $injuredUsersNicks"
    }
    promptNextUser(shootResult.some)
  }

  private def nextUser = players(game.checkCurrentPlayer.id)

  private def nextUserPrompt = s"Your turn ${nextUser.mention}"

  private def promptNextUser(message: Option[String] = None): SendMessage = {
    val keyboard = ReplyKeyboardMarkup(
      Array(
        Array(KeyboardButton("/up"), KeyboardButton("/down"), KeyboardButton("/left"), KeyboardButton("/right")),
        Array(KeyboardButton("/shootup"), KeyboardButton("/shootdown"), KeyboardButton("/shootleft"), KeyboardButton("/shootright"))))
    val text = message.fold("")(m => s"$m\n\n") + nextUserPrompt
    SendMessage(chatId, text, replyMarkup = Some(keyboard))
  }
}
