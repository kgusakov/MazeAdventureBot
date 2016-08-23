package com.maze.bot

import java.io.ByteArrayOutputStream

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}
import com.maze.bot.GameMaster.{Move, Shoot}
import com.maze.bot.telegram.api.{SendMessage, TelegramApiClient, User}
import com.maze.game.Directions.Direction
import com.maze.game.{Drawer, Game}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class GameRouter(apiClient: TelegramApiClient) extends Actor with ActorLogging {

  import GameRouter._
  import resource._

  private val pendingGames = mutable.HashMap[Int, PendingGame]()
  private val activeGames = mutable.HashMap[Int, ActiveGame]()

  override def receive: Receive = {
    case NewGame(chatId, user) =>
      if ((pendingGames contains chatId) || (activeGames contains chatId))
        apiClient.sendMessage(SendMessage(chatId, "This chat already have one game"))
      else {
        pendingGames += chatId -> PendingGame(chatId, user, ArrayBuffer(user))
        apiClient.sendMessage(SendMessage(chatId, "Game successfully created, waiting for players"))
      }
    case JoinGame(chatId, user) =>
      if (pendingGames contains chatId) {
        pendingGames(chatId).players += user
        apiClient.sendMessage(SendMessage(chatId, "Welcome to game! Waiting for other players"))
      } else if (activeGames contains chatId) {
        apiClient.sendMessage(SendMessage(chatId, "Game already started, sorry"))
      } else
        apiClient.sendMessage(SendMessage(chatId, "There are no any games to join in current chat"))
    case StartGame(chatId, user) =>
      if (pendingGames contains chatId) {
        if (user == pendingGames(chatId).author) {
          apiClient.sendMessage(SendMessage(chatId, "Game started, let it bleed!"))
          activeGames += chatId -> {
            val game = pendingGames(chatId)
            ActiveGame(user.id, context.system.actorOf(Props(
              new GameMaster(apiClient, chatId, game.players.map(p => p.id -> p).toMap))))
          }
        } else {
          apiClient.sendMessage(SendMessage(chatId, "Only creator of the game can start it"))
        }
      }
      else
        apiClient.sendMessage(SendMessage(chatId, "There are no pending games to start"))
    case EndGame(chatId, userId) =>
      if ((pendingGames contains chatId) || (activeGames contains chatId))
        if (pendingGames.get(chatId).fold(false)(_.author == userId) ||
          activeGames.get(chatId).fold(false)(_.author == userId)) {
          activeGames.get(chatId).foreach {
            _.game ! PoisonPill
          }
          activeGames -= chatId
          pendingGames -= chatId
          apiClient.sendMessage(SendMessage(chatId, "Game finished"))
        } else {
          apiClient.sendMessage(SendMessage(chatId, "Only author of the game can finish it"))
        }
    case WinGame(chatId, snapshot, winner) =>
      activeGames.get(chatId).foreach(_.game ! PoisonPill)
      activeGames -= chatId
      sendStartingPositionsPicture(snapshot, chatId)
      apiClient.sendMessage(SendMessage(chatId, s"We have a winner: @${winner.username}"))
    case MoveAction(chatId, user, direction) =>
      activeGames.get(chatId) match {
        case Some(game) => game.game ! Move(user, direction)
        case None => apiClient.sendMessage(SendMessage(chatId, "There is no games in this chat"))
      }

    case ShootAction(chatId, user, direction) =>
      activeGames.get(chatId) match {
        case Some(game) => game.game ! Shoot(user, direction)
        case None => apiClient.sendMessage(SendMessage(chatId, "There is no games in this chat"))
      }
  }

  private def sendStartingPositionsPicture(snapshot: Game, chatId: Int) =
    for (output <- managed(new ByteArrayOutputStream())) {
      Drawer.drawGame(snapshot, output)
      apiClient.sendPhoto(chatId, output.toByteArray)
    }
}

object GameRouter {
  case class NewGame(chatId: Int, user: User)
  case class JoinGame(chatId: Int, user: User)
  case class StartGame(chatId: Int, user: User)
  case class EndGame(chatId: Int, user: User)
  case class MoveAction(chatId: Int, user: User, direction: Direction)
  case class ShootAction(chatId: Int, user: User, direction: Direction)
  case class WinGame(chatId: Int, snapshot: Game, winner: User)

  private case class PendingGame(chatId: Int, author: User, players: ArrayBuffer[User])
  private case class ActiveGame(author: Int, game: ActorRef)
}

