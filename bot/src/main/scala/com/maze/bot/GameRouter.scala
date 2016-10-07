package com.maze.bot

import java.io.ByteArrayOutputStream

import akka.actor.{ActorLogging, ActorRef}
import akka.persistence.PersistentActor
import com.maze.bot.GameMaster.WinGame
import com.maze.bot.telegram.api.{SendMessage, SendPhoto, User}
import com.maze.game.Directions.Direction
import com.maze.game.{Drawer, Game}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class GameRouter(messageSender: ActorRef) extends PersistentActor with ActorLogging {

  import GameRouter._
  import resource._

  private val pendingGames = mutable.HashMap[Int, PendingGame]()
  private val activeGames = mutable.HashMap[Int, ActiveGame]()

  override def receiveCommand: Receive = {
    case NewGame(chatId, user) =>
      if ((pendingGames contains chatId) || (activeGames contains chatId))
        messageSender ! SendMessage(chatId, "This chat already have one game")
      else {
        persistAndProcessEvent(NewGameEvent(chatId, user))
        messageSender ! SendMessage(chatId, "Game successfully created, waiting for players")
      }
    case JoinGame(chatId, user) =>
      if (pendingGames contains chatId) {
        persistAndProcessEvent(JoinGameEvent(chatId, user))
        messageSender ! SendMessage(chatId, "Welcome to game! Waiting for other players")
      } else if (activeGames contains chatId) {
        messageSender ! SendMessage(chatId, "Game already started, sorry")
      } else
        messageSender ! SendMessage(chatId, "There are no any games to join in current chat")
    case StartGame(chatId, user) =>
      if (pendingGames contains chatId) {
        if (user == pendingGames(chatId).author) {
          messageSender ! SendMessage(chatId, "Game started, let it bleed!")
          val gameMaster = GameMaster(chatId, pendingGames(chatId).players.map(p => p.id -> p).toMap)
          val startMessage = gameMaster.start
          persistAndProcessEvent(StartedGameEvent(gameMaster, startMessage, chatId, user))
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
          persistAndProcessEvent(EndGameEvent(chatId, userId))
          messageSender ! SendMessage(chatId, "Game finished")
        } else {
          messageSender ! SendMessage(chatId, "Only author of the game can finish it")
        }
    case MoveAction(chatId, user, direction) =>
      activeGames.get(chatId) match {
        case Some(game) =>
          persistAndProcessEvent(MoveActionEvent(chatId, user, direction))
        case None => messageSender ! SendMessage(chatId, "There is no games in this chat")
      }
    case ShootAction(chatId, user, direction) =>
      activeGames.get(chatId) match {
        case Some(game) =>
          persistAndProcessEvent(ShootActionEvent(chatId, user, direction))
        case None => messageSender ! SendMessage(chatId, "There is no games in this chat")
      }
  }

  private def processEvent(evt: Event): Unit = evt match {
    case NewGameEvent(chatId, user) =>
      pendingGames += chatId -> PendingGame(chatId, user, ArrayBuffer(user))
    case JoinGameEvent(chatId, user) =>
      pendingGames(chatId).players += user
    case StartedGameEvent(gameMaster, startMessage, chatId, user) =>
      activeGames += chatId -> {
        if (recoveryFinished) messageSender ! startMessage
        ActiveGame(user.id, gameMaster)
      }
      pendingGames -= chatId
    case EndGameEvent(chatId, user) =>
      activeGames -= chatId
      pendingGames -= chatId
    case MoveActionEvent(chatId, user, direction) =>
      activeGames(chatId).gameMaster.move(user, direction) match {
        case Left(WinGame(snapshot, winner)) =>
          activeGames -= chatId
          if (recoveryFinished) {
            sendStartingPositionsPicture(snapshot, chatId)
            messageSender ! SendMessage(chatId, s"We have a winner: @${winner.username}")
          }
        case Right(sendMessage) => if (recoveryFinished) messageSender ! sendMessage
      }
    case ShootActionEvent(chatId, user, direction) =>
      messageSender ! activeGames(chatId).gameMaster.shoot(user, direction)
  }

  override def receiveRecover: Receive = {
    case evt: Event => processEvent(evt)
  }

  private def persistAndProcessEvent(evt: Event) = persist(evt){ e => processEvent(e)}

  private def sendStartingPositionsPicture(snapshot: Game, chatId: Int) =
    for (output <- managed(new ByteArrayOutputStream())) {
      Drawer.drawGame(snapshot, output)
      messageSender ! SendPhoto(chatId, output.toByteArray)
    }

  override def persistenceId: String = "game-router"
}

object GameRouter {
  case class NewGame(chatId: Int, user: User)
  case class JoinGame(chatId: Int, user: User)
  case class StartGame(chatId: Int, user: User)
  case class EndGame(chatId: Int, user: User)
  case class MoveAction(chatId: Int, user: User, direction: Direction)
  case class ShootAction(chatId: Int, user: User, direction: Direction)

  trait Event
  case class NewGameEvent(chatId: Int, user: User) extends Event
  case class JoinGameEvent(chatId: Int, user: User) extends Event
  case class StartedGameEvent(gameMaster: GameMaster, startMessage: SendMessage, chatId: Int, user: User) extends Event
  case class EndGameEvent(chatId: Int, user: User) extends Event
  case class MoveActionEvent(chatId: Int, user: User, direction: Direction) extends Event
  case class ShootActionEvent(chatId: Int, user: User, direction: Direction) extends Event


  private case class PendingGame(chatId: Int, author: User, players: ArrayBuffer[User])
  private case class ActiveGame(author: Int, gameMaster: GameMaster)
}

