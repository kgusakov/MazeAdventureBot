package com.maze.bot

import java.io._
import java.util.Properties

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import com.maze.bot.telegram.api.{Message, SendMessage, TelegramApiClient, User}
import com.maze.bot.telegram.api.TelegramApiClient._
import com.maze.game._
import com.maze.game.Directions.Direction
import com.maze.game.Items.{Armory, Chest, Exit, Hospital}
import com.maze.game.MovementResults.{NewCell, Wall, Win}
import com.maze.game.ShootResults.{Injured, Miss}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Random
import scalaz.Scalaz._

object Bot extends App with LazyLogging {

  private val updateIdProp: String = "update_id"
  var updateId = PropsStore.properties.getProperty(updateIdProp, "0").toInt

  val actorSystem = ActorSystem()
  val gameManager = actorSystem.actorOf(Props[GameManager])

  import scala.concurrent.ExecutionContext.Implicits.global

  def toMove(input: String): Option[Direction] = {
      input.split("@")(0) match {
        case "/up" => Directions.Up.some
        case "/down" => Directions.Down.some
        case "/right" => Directions.Right.some
        case "/left" => Directions.Left.some
        case _ => none
      }
  }

  def toShoot(input: String): Option[Direction] = {
    input.split("@")(0) match {
      case "/shootup" => Directions.Up.some
      case "/shootdown" => Directions.Down.some
      case "/shootright" => Directions.Right.some
      case "/shootleft" => Directions.Left.some
      case _ => none
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
              case Message(_, from, chat, _, Some(text), _) if toMove(text).isDefined =>
                toMove(text).foreach(d => gameManager ! MoveAction(chat.id, from, d))
              case Message(_, from, chat, _, Some(text), _) if toShoot(text).isDefined =>
                toShoot(text).foreach(d => gameManager ! ShootAction(chat.id, from, d))
              case message@Message(_, _, chat, _, _, _) =>
                logger.warn(message.toString)
                TelegramApiClient.sendMessage(SendMessage(chat.id, "Wrong command"))
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

  case class ShootAction(chatId: Int, user: User, direction: Direction)

  case class WinGame(chatId: Int, snapshot: Game, winner: User)

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
        sendStartingPositionsPicture(snapshot, chatId)
        sendMessage(SendMessage(chatId, s"We have a winner: @${winner.username}"))
      case MoveAction(chatId, user, direction) =>
        activeGames.get(chatId) match {
          case Some(game) => game.game ! Move(user, direction)
          case None => sendMessage(SendMessage(chatId, "There is no games in this chat"))
        }

      case ShootAction(chatId, user, direction) =>
        activeGames.get(chatId) match {
          case Some(game) => game.game ! Shoot(user, direction)
          case None => sendMessage(SendMessage(chatId, "There is no games in this chat"))
        }
    }

    private def sendStartingPositionsPicture(snapshot: Game, chatId: Int) =
      for (output <- managed(new ByteArrayOutputStream())) {
        Drawer.drawGame(snapshot, output)
        sendPhoto(chatId, output.toByteArray)
      }
  }

  case class Move(user: User, direction: Direction)
  case class Shoot(user: User, direction: Direction)

  class GameMaster(chatId: Int, players: Map[Int, User]) extends Actor {
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