package com.maze.bot

import java.io.ByteArrayOutputStream

import akka.actor.{Actor, ActorLogging, ActorRef}
import com.maze.bot.GameMaster.WinGame
import com.maze.bot.telegram.api.{SendMessage, SendPhoto, User}
import com.maze.game.Directions.Direction
import com.maze.game.{Drawer, Game}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class GameRouter(messageSender: ActorRef) extends Actor with ActorLogging {

  import GameRouter._
  import resource._

  private val pendingGames = mutable.HashMap[Int, PendingGame]()
  private val activeGames = mutable.HashMap[Int, ActiveGame]()

  override def receive: Receive = {
    case NewGame(chatId, user) =>
      if ((pendingGames contains chatId) || (activeGames contains chatId))
        messageSender ! SendMessage(chatId, "This chat already have one game")
      else {
        pendingGames += chatId -> PendingGame(chatId, user, ArrayBuffer(user))
        messageSender ! SendMessage(chatId, "Game successfully created, waiting for players")
      }
    case JoinGame(chatId, user) =>
      if (pendingGames contains chatId) {
        pendingGames(chatId).players += user
        messageSender ! SendMessage(chatId, "Welcome to game! Waiting for other players")
      } else if (activeGames contains chatId) {
        messageSender ! SendMessage(chatId, "Game already started, sorry")
      } else
        messageSender ! SendMessage(chatId, "There are no any games to join in current chat")
    case StartGame(chatId, user) =>
      if (pendingGames contains chatId) {
        if (user == pendingGames(chatId).author) {
          messageSender ! SendMessage(chatId, "Game started, let it bleed!")
          activeGames += chatId -> {
            val game = pendingGames(chatId)
            val gameMaster = GameMaster(chatId, game.players.map(p => p.id -> p).toMap)
            messageSender ! gameMaster.start
            ActiveGame(user.id, gameMaster)
          }
        } else {
          messageSender ! SendMessage(chatId, "Only creator of the game can start it")
        }
      }
      else
        messageSender ! SendMessage(chatId, "There are no pending games to start")
    case EndGame(chatId, userId) =>
      if ((pendingGames contains chatId) || (activeGames contains chatId))
        if (pendingGames.get(chatId).fold(false)(_.author == userId) ||
          activeGames.get(chatId).fold(false)(_.author == userId)) {
          activeGames -= chatId
          pendingGames -= chatId
          messageSender ! SendMessage(chatId, "Game finished")
        } else {
          messageSender ! SendMessage(chatId, "Only author of the game can finish it")
        }
    case MoveAction(chatId, user, direction) =>
      activeGames.get(chatId) match {
        case Some(game) =>
          game.gameMaster.move(user, direction) match {
            case Left(WinGame(snapshot, winner)) =>
              activeGames -= chatId
              sendStartingPositionsPicture(snapshot, chatId)
              messageSender ! SendMessage(chatId, s"We have a winner: @${winner.username}")
            case Right(sendMessage) => messageSender ! sendMessage
          }
        case None => messageSender ! SendMessage(chatId, "There is no games in this chat")
      }

    case ShootAction(chatId, user, direction) =>
      activeGames.get(chatId) match {
        case Some(game) =>
          messageSender ! game.gameMaster.shoot(user, direction)
        case None => messageSender ! SendMessage(chatId, "There is no games in this chat")
      }
  }

  private def sendStartingPositionsPicture(snapshot: Game, chatId: Int) =
    for (output <- managed(new ByteArrayOutputStream())) {
      Drawer.drawGame(snapshot, output)
      messageSender ! SendPhoto(chatId, output.toByteArray)
    }
}

object GameRouter {
  case class NewGame(chatId: Int, user: User)
  case class JoinGame(chatId: Int, user: User)
  case class StartGame(chatId: Int, user: User)
  case class EndGame(chatId: Int, user: User)
  case class MoveAction(chatId: Int, user: User, direction: Direction)
  case class ShootAction(chatId: Int, user: User, direction: Direction)


  private case class PendingGame(chatId: Int, author: User, players: ArrayBuffer[User])
  private case class ActiveGame(author: Int, gameMaster: GameMaster)
}

