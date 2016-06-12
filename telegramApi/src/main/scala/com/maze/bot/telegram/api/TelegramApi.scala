package com.maze.bot.telegram.api

import java.net.URL
import java.nio.charset.{Charset, StandardCharsets}

import argonaut._
import Argonaut._
import com.stackmob.newman.{ApacheHttpClient, Headers}
import com.stackmob.newman.response.HttpResponse
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.Future

case class Update(updateId: Int,
                  message: Message)
object Update {
  implicit def UpdateCodecJson: CodecJson[Update] =
    casecodec2(Update.apply, Update.unapply)("update_id", "message")
}

case class Response(ok: Boolean, result: List[Update])
object Response {
  implicit def ResponseCodecJson: CodecJson[Response] =
    casecodec2(Response.apply, Response.unapply)("ok", "result")
}

case class MessageEntity(`type`: String, offset: Long, length: Long)
object MessageEntity {
  implicit def MessageEntityCodecJson: CodecJson[MessageEntity] =
    casecodec3(MessageEntity.apply, MessageEntity.unapply)("type", "offset", "length")
}

case class Message(messageId: Int, from: User, chat: Chat, date: Long, text: String, entities: Option[List[MessageEntity]] = None) {

  lazy val isCommand = entities.fold(false)(_.exists(m => m.`type` == "bot_command"))
}
object Message {
  implicit def MessageCodecJson: CodecJson[Message] =
    CodecJson(
      (m: Message) =>
        ("message_id" := m.messageId) ->:
        ("from" := m.from) ->:
        ("chat" := m.chat) ->:
        ("date" := m.date) ->:
        ("text" := m.text) ->:
        m.entities.map("entities" := _) ->?:
        jEmptyObject,
      c => for {
        messageId <- (c --\ "message_id").as[Int]
        from <- (c --\ "from").as[User]
        chat <- (c --\ "chat").as[Chat]
        text <- (c --\ "text").as[String]
        date <- (c --\ "date").as[Long]
        entities <- (c --\ "entities").as[Option[List[MessageEntity]]]
    } yield Message(messageId, from, chat, date, text, entities))
}

case class Chat(id: Int, firstName: Option[String] = None, lastName: Option[String] = None, username: Option[String], chatType: String, title: Option[String] = None)
object Chat {

  implicit def ChatCodecJson: CodecJson[Chat] = CodecJson (
    (chat: Chat) =>
      ("id" := chat.id) ->:
      chat.firstName.map("first_name" := _) ->?:
      chat.lastName.map("last_name" := _) ->?:
      chat.username.map("username" := _) ->?:
      ("type" := chat.chatType) ->:
      chat.title.map("title" := _) ->?:
      jEmptyObject,
    c => for {
      id <- (c --\ "id").as[Int]
      firstName <- (c --\ "first_name").as[Option[String]]
      lastName <- (c --\ "last_name").as[Option[String]]
      username <- (c --\ "username").as[Option[String]]
      chatType <- (c --\ "type").as[String]
      title <- (c --\ "title").as[Option[String]]
    } yield Chat(id, firstName, lastName, username, chatType, title)
  )

}

case class User(id: Int, firstName: String, lastName: String, username: String)
object User {
  implicit def UserCodecJson: CodecJson[User] =
    casecodec4(User.apply, User.unapply)("id", "first_name", "last_name", "username")
}

case class SendMessage(chatId: Int,
                       text: String,
                       parseMode: Option[String] = None,
                       disableWebPagePreview: Option[Boolean] = None,
                       disableNotification: Option[Boolean] = None,
                       replyToMessageId: Option[Int] = None,
                       replyMarkup: Option[String] = None)
object SendMessage {
  implicit def SendMessageCodecJson: EncodeJson[SendMessage] = EncodeJson{(m: SendMessage) =>
    ("chat_id" := m.chatId) ->:
      ("text" := m.text) ->:
      m.parseMode.map("parse_mode" := _) ->?:
      m.disableWebPagePreview.map("disable_web_page_preview" := _) ->?:
      m.disableNotification.map("disable_notification" := _) ->?:
      m.replyToMessageId.map("reply_to_message_id" := _) ->?:
      m.replyMarkup.map("reply_markup" := _) ->?: jEmptyObject
  }
}

object TelegramApiClient extends LazyLogging {
  import com.stackmob.newman.dsl._
  import argonaut._, Argonaut._


  private val baseUri = "https://api.telegram.org/bot180715621:AAFUc2sYIfo4FYww_01v3VGZXiB_WcPPL4k"
  private val getUpdatesUri = s"$baseUri/getUpdates"
  private val sendMessageUri = s"$baseUri/sendMessage"

  implicit private val httpClient = new ApacheHttpClient
  import scala.concurrent.ExecutionContext.Implicits.global

  def getUpdates(updateId: Int = 0): Future[Option[List[Update]]] = {
    GET(new URL(s"${getUpdatesUri}?offset=${updateId}")).apply.map { response =>
      logger.debug(response.bodyString)
      response.bodyString(StandardCharsets.UTF_8).decodeValidation[Response].fold(
        {errorMessage =>
          logger.error(errorMessage)
          None
        },
        r => Some(r.result))
    }
  }

  def sendMessage(message: SendMessage): Future[HttpResponse] = {
    POST(new URL(sendMessageUri))
      .addHeaders(Headers(("Content-Type", "application/json")))
      .setBodyString(message.asJson.nospaces).apply
  }
}


