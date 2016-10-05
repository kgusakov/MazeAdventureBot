package com.maze.bot

import akka.actor.{Actor, ActorLogging}
import akka.http.scaladsl.model.StatusCodes
import com.maze.bot.telegram.api.{SendMessage, SendPhoto, TelegramApiClient}

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

class MessageSender(apiClient: TelegramApiClient) extends Actor with ActorLogging {

  override def receive: Receive = {
    case sendMessage: SendMessage =>
      apiClient.sendMessage(sendMessage).onComplete {
        case Success(httpResp) =>
          if (httpResp.status != StatusCodes.OK) log.debug(s"Status code in response of $sendMessage request is not ok:" + httpResp)
        case Failure(t) => log.error(s"While processing request $sendMessage error occuried", t)
      }
    case sendPhoto: SendPhoto =>
      apiClient.sendPhoto(sendPhoto).onComplete {
        case Success(httpResp) =>
          if (httpResp.status != StatusCodes.OK) log.debug(s"Status code in response of $sendPhoto request is not ok:" + httpResp)
        case Failure(t) => log.error(s"While processing request $sendPhoto error occuried", t)
      }
  }

}
