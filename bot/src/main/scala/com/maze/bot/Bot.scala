package com.maze.bot

import java.io._
import java.util.Properties

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import com.maze.bot.telegram.api.{Message, SendMessage, TelegramApiClient, User}
import com.maze.bot.telegram.api.TelegramApiClient._
import com.maze.game.{Directions, Drawer, Game, Maze}
import com.maze.game.Directions.Direction
import com.maze.game.Items.{Chest, Exit}
import com.maze.game.Results.{NewCell, NotYourTurn, Wall, Win}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scalaz.Scalaz._

object Bot extends App with LazyLogging {

  private val updateIdProp: String = "update_id"
  var updateId = PropsStore.properties.getProperty(updateIdProp, "0").toInt

  val actorSystem = ActorSystem()
  val gameManager = actorSystem.actorOf(Props[GameManager])

  import scala.concurrent.ExecutionContext.Implicits.global

  def toMove(input: String): Either[String, Direction] = {
      input.split("@")(0) match {
        case "/up" => Right(Directions.Up)
        case "/down" => Right(Directions.Down)
        case "/right" => Right(Directions.Right)
        case "/left" => Right(Directions.Left)
        case _ => Left("Wrong direction")
      }
  }

  while (true) {
    try {
      Await.result(TelegramApiClient.getUpdates(updateId), 5 minute) match {
        case Some(updates) => {
          updates.filter(_.message.isCommand).foreach { update =>
            logger.info("Received update: " + update)
            update.message match {
              case Message(_, from, chat, _, Some(text), _) if text.startsWith("/new") =>
                gameManager ! NewGame(chat.id, from)
              case Message(_, from, chat, _, Some(text), _) if text.startsWith("/join") =>
                gameManager ! JoinGame(chat.id, from)
              case Message(_, from, chat, _, Some(text), _) if text.startsWith("/start") =>
                gameManager ! StartGame(chat.id, from)
              case Message(_, from, chat, _, Some(text), _) if text.startsWith("/end") =>
                gameManager ! EndGame(chat.id, from)
              case Message(_, from, chat, _, Some(text), _) if toMove(text).isRight =>
                toMove(text) match {
                  case Left(errorMessage) => TelegramApiClient.sendMessage(SendMessage(chat.id, errorMessage))
                  case Right(direction) => gameManager ! MoveAction(chat.id, from, direction)
                }
              case message@Message(_, _, _, _, _, _) =>
                logger.warn(message.toString)
            }
          }
          val nextUpdateId = updates.map(_.updateId).fold(0)((a, b) => math.max(a, b))
          if (nextUpdateId >= updateId) {
            updateId = nextUpdateId + 1
            PropsStore.save(updateIdProp, updateId.toString)
            logger.debug(updateId.toString)
          }
        }
        case None =>
      }
    } catch {
      case _: Throwable =>
    }

  }

  case class NewGame(chatId: Int, user: User)

  case class JoinGame(chatId: Int, user: User)

  case class StartGame(chatId: Int, user: User)

  case class EndGame(chatId: Int, user: User)

  case class MoveAction(chatId: Int, user: User, direction: Direction)

  case class WinGame(chatId: Int, snapshot: Maze, winner: User)

  case class PendingGame(chatId: Int, author: User, players: ArrayBuffer[User])

  case class ActiveGame(author: Int, game: ActorRef)

  class GameManager extends Actor with ActorLogging {

    import resource._

    val pendingGames = mutable.HashMap[Int, PendingGame]()
    val activeGames = mutable.HashMap[Int, ActiveGame]()

    override def receive: Receive = {
      case NewGame(chatId, user) =>
        if ((pendingGames contains chatId) || (activeGames contains chatId))
          sendMessage(SendMessage(chatId, "This chat already have one game"))
        else {
          pendingGames += chatId -> PendingGame(chatId, user, ArrayBuffer(user))
          sendMessage(SendMessage(chatId, "Game successfully created, waiting for players"))
        }
      case JoinGame(chatId, user) =>
        if (pendingGames contains chatId) {
          pendingGames(chatId).players += user
          sendMessage(SendMessage(chatId, "Welcome to game! Waiting for other players"))
        } else if (activeGames contains chatId) {
          sendMessage(SendMessage(chatId, "Game already started, sorry"))
        } else
          sendMessage(SendMessage(chatId, "There are no any games to join in current chat"))
      case StartGame(chatId, user) =>
        if (pendingGames contains chatId) {
          if (user == pendingGames(chatId).author) {
            sendMessage(SendMessage(chatId, "Game started, let it bleed!"))
            activeGames += chatId -> {
              val game = pendingGames(chatId)
              ActiveGame(user.id, context.system.actorOf(Props(
                new GameMaster(chatId, game.players.map(p => p.id -> p).toMap))))
            }
          } else {
            sendMessage(SendMessage(chatId, "Only creator of the game can start it"))
          }
        }
        else
          sendMessage(SendMessage(chatId, "There are no pending games to start"))
      case EndGame(chatId, userId) =>
        if ((pendingGames contains chatId) || (activeGames contains chatId))
          if (pendingGames.get(chatId).fold(false)(_.author == userId) ||
            activeGames.get(chatId).fold(false)(_.author == userId)) {
            activeGames.get(chatId).foreach {
              _.game ! PoisonPill
            }
            activeGames -= chatId
            pendingGames -= chatId
            sendMessage(SendMessage(chatId, "Game finished"))
          } else {
            sendMessage(SendMessage(chatId, "Only author of the game can finish it"))
          }
      case WinGame(chatId, snapshot, winner) =>
        activeGames.get(chatId).foreach(_.game ! PoisonPill)
        activeGames -= chatId
        for (output <- managed(new ByteArrayOutputStream())) {
          Drawer.drawMaze(snapshot, output)
          sendPhoto(chatId, output.toByteArray)
        }
        sendMessage(SendMessage(chatId, "We have a winner"))
      case MoveAction(chatId, user, direction) =>
        activeGames.get(chatId) match {
          case Some(game) => game.game ! Move(user, direction)
          case None => sendMessage(SendMessage(chatId, "There is no games in this chat"))
        }
    }
  }

  case class Move(user: User, direction: Direction)

  class GameMaster(chatId: Int, players: Map[Int, User]) extends Actor {
    val game = new Game(SortedSet(players.keySet.toList: _*))

    override def receive: Receive = {
      case Move(user, direction) =>
        val message: Option[String] = game.move(user.id, direction) match {
          case NotYourTurn => "Sorry, not your turn".some
          case NewCell(items) =>
            if (items isEmpty) "You moved to empty cell ".some
            else ("You found following items: " + items.map {
              case Exit => "Exit"
              case Chest => "Chest"
            }.mkString(",")).some
          case Wall => "Sorry dude, there is a wall".some
          case Win(playerId) =>
            sender() ! WinGame(chatId, game.initialSnapshot, players(playerId))
            none
        }
        for (m <- message) TelegramApiClient sendMessage SendMessage(chatId, m)
    }
  }

}

object PropsStore {

  import resource._

  private val fileName = "props.store"

  val properties = new Properties()

  for (reader <- managed(Source.fromFile(fileName).reader())) properties.load(reader)

  def save(key: String, value: String): Unit = {
    properties.put(key, value)
    for (out <- managed(new FileOutputStream(fileName))) properties.store(out, null)
  }
}