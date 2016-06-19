package com.maze.bot

import java.io.{FileOutputStream, OutputStream, Reader}
import java.util.Properties

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import com.maze.bot.telegram.api.{Message, SendMessage, TelegramApiClient}
import com.maze.bot.telegram.api.TelegramApiClient._
import com.maze.game.{Directions, Game}
import com.maze.game.Directions.Direction
import com.maze.game.Items.{Chest, Exit}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source

object Bot extends App with LazyLogging  {

  private val updateIdProp: String = "update_id"
  var updateId = PropsStore.properties.getProperty(updateIdProp, "0").toInt

  val actorSystem = ActorSystem()
  val gameManager = actorSystem.actorOf(Props[GameManager])

  import scala.concurrent.ExecutionContext.Implicits.global

  def toMove(input: String): Either[String, Direction] = {
    val splitted = input.split(" ")
    if (splitted.length == 2)
      splitted(1) match {
        case "up" => Right(Directions.Up)
        case "down" => Right(Directions.Down)
        case "right" => Right(Directions.Right)
        case "left" => Right(Directions.Left)
        case _ => Left("Wrong direction")
      }
    else Left("Wrong direction")
  }

  while(true) {
    Await.result(TelegramApiClient.getUpdates(updateId), 1 minute) match {
          case Some(updates) => {
            updates.filter(_.message.isCommand).foreach { update =>
              logger.info("Received update: " + update)
              update.message match {
                case Message(_, from, chat, _, Some(text), _) if text.startsWith("/new") =>
                  gameManager ! NewGame(chat.id, from.id)
                case Message(_, from, chat, _, Some(text), _) if text.startsWith("/join") =>
                  gameManager ! JoinGame(chat.id, from.id)
                case Message(_, from, chat, _, Some(text), _) if text.startsWith("/start") =>
                  gameManager ! StartGame(chat.id, from.id)
                case Message(_, from, chat, _, Some(text), _) if text.startsWith("/end") =>
                  gameManager ! EndGame(chat.id, from.id)
                case Message(_, from, chat, _, Some(text), _) if text.startsWith("/move") =>
                  toMove(text) match {
                    case Left(errorMessage) => TelegramApiClient.sendMessage(SendMessage(chat.id, errorMessage))
                    case Right(direction) => gameManager ! MoveAction(chat.id, from.id, direction)
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
    }

  case class NewGame(chatId: Int, userId: Int)
  case class JoinGame(chatId: Int, userId: Int)
  case class StartGame(chatId: Int, userId: Int)
  case class EndGame(chatId: Int, userId: Int)
  case class MoveAction(chatId: Int, userId: Int, direction: Direction)

  case class PendingGame(chatId: Int, author: Int, players: mutable.ArrayBuffer[Int])
  case class ActiveGame(author: Int, game: ActorRef)

  class GameManager extends Actor with ActorLogging {

    val pendingGames = mutable.HashMap[Int, PendingGame]()
    val activeGames = mutable.HashMap[Int, ActiveGame]()

    override def receive: Receive = {
      case NewGame(chatId, userId) =>
        if ((pendingGames contains chatId) || (activeGames contains chatId))
          sendMessage (SendMessage(chatId, "This chat already have one game"))
        else {
          pendingGames += chatId -> PendingGame(chatId, userId, ArrayBuffer(userId))
          sendMessage (SendMessage(chatId, "Game successfully created, waiting for players"))
        }
      case JoinGame(chatId, userId) =>
        if (pendingGames contains chatId) {
          pendingGames(chatId).players += userId
          sendMessage (SendMessage(chatId, "Welcome to game! Waiting for other players"))
        } else if (activeGames contains chatId) {
          sendMessage (SendMessage(chatId, "Game already started, sorry"))
        } else
          sendMessage (SendMessage(chatId, "There are no any games to join in current chat"))
      case StartGame(chatId, userId) =>
        if (pendingGames contains chatId) {
          if (userId == pendingGames(chatId).author) {
            sendMessage (SendMessage(chatId, "Game started, let it bleed!"))
            activeGames += chatId -> {
              val game = pendingGames(chatId)
              ActiveGame(userId, context.system.actorOf(Props(new GameMaster(chatId, game.players.toSet))))
            }
          } else {
            sendMessage (SendMessage(chatId, "Only creator of the game can start it"))
          }
        }
        else
          sendMessage (SendMessage(chatId, "There are no pending games to start"))
      case EndGame(chatId, userId) =>
        if ((pendingGames contains chatId) || (activeGames contains chatId))
          if (pendingGames.get(chatId).fold(false)(_.author == userId) ||
            activeGames.get(chatId).fold(false)(_.author == userId)) {
            activeGames.get(chatId).foreach{ _.game ! PoisonPill }
            activeGames -= chatId
            pendingGames -= chatId
            sendMessage (SendMessage(chatId, "Game finished"))
          } else {
            sendMessage (SendMessage(chatId, "Only author of the game can finish it"))
          }
      case MoveAction(chatId, userId, direction) =>
        activeGames.get(chatId) match {
          case Some(game) => game.game ! Move(userId, direction)
          case None => sendMessage (SendMessage(chatId, "There is no games in this chat"))
        }
    }
  }

  case class Move(playerId: Int, direction: Direction)

  class GameMaster(chatId: Int, players: Set[Int]) extends Actor {
    val game = new Game(SortedSet(players.toList:_*))

    override def receive: Receive = {
      case Move(playerId, direction) =>
        val message = game.move(playerId, direction) match {
          case Left(message) => message
          case Right(Some(cell)) =>
            "You found following items: " + cell.item.map {
              case Exit => "Exit"
              case Chest => "Chest"
            }.mkString(",")
          case Right(None) =>
            "Sorry dude, there is a wall"
        }
        TelegramApiClient sendMessage SendMessage(chatId, message)
    }
  }
}

object PropsStore {

  private val fileName = "props.store"

  val properties = new Properties()

  {
    var reader: Reader = null
    try {
      reader = Source.fromFile(fileName).reader()
      properties.load(reader)
    } catch {
      case _ =>
    } finally {
      if (reader != null) reader close
    }
  }

  def save(key: String, value: String): Unit = {
    properties.put(key, value)
    var out: OutputStream = null
    try {
      out = new FileOutputStream(fileName)
      properties.store(out, null)
    } finally {
      out close
    }

  }
}