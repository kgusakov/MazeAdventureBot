package com.maze.bot

import akka.actor.{ActorSystem, Props}
import com.maze.bot.telegram.api.{Message, SendMessage, TelegramApiClient}
import com.maze.game._
import com.maze.game.Directions.Direction
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.Await
import scala.concurrent.duration._
import scalaz.Scalaz._
import GameRouter._

object Bot extends App with LazyLogging {

  private val updateIdProp: String = "update_id"
  val apiClient = new TelegramApiClient(args(0))
  var updateId = PropsStore.properties.getProperty(updateIdProp, "0").toInt

  val actorSystem = ActorSystem()
  val gameManager = actorSystem.actorOf(Props(new GameRouter(apiClient)))

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
      Await.result(apiClient.getUpdates(updateId), 5 minute) match {
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
                apiClient.sendMessage(SendMessage(chat.id, "Wrong command"))
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
      case t: Throwable => logger.error("Unknown error occuried", t)
    }
  }

}

