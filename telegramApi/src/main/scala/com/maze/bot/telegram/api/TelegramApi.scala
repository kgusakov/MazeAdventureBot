package com.maze.bot.telegram.api

import java.io.{File, InputStream}
import java.nio.charset.StandardCharsets

import argonaut._
import Argonaut._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model.{HttpEntity, _}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class Update(updateId: Int,
                  message: Message)
object Update {

  implicit def UpdateCodecJson: CodecJson[Update] = CodecJson (
    (update: Update) =>
      ("update_id" := update.updateId) ->:
        ("message" := update.message) ->:
        jEmptyObject,
    u => for {
      id <- (u --\ "update_id").as[Int]
      message <- messageExtract(u).as[Message]
    } yield Update(id, message))

  def messageExtract(hCursor: HCursor): ACursor = {
    val cursor = hCursor --\ "message"
    if (cursor.failed)
      hCursor --\ "edited_message"
    else cursor
  }
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

case class KeyboardButton(text: String)
object KeyboardButton {
  implicit def KeyboardButtonCodecJson: CodecJson[KeyboardButton] =
    casecodec1(KeyboardButton.apply, KeyboardButton.unapply)("text")
}

case class ReplyKeyboardMarkup(keyboard: Array[Array[KeyboardButton]])
object ReplyKeyboardMarkup {
  implicit def ReplyKeyboardMarkupCodecJson: CodecJson[ReplyKeyboardMarkup] =
    casecodec1(ReplyKeyboardMarkup.apply, ReplyKeyboardMarkup.unapply)("keyboard")
}

case class Message(messageId: Int, from: User, chat: Chat, date: Long, text: Option[String], entities: Option[List[MessageEntity]] = None) {

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
        m.text.map("text" := _) ->?:
        m.entities.map("entities" := _) ->?:
        jEmptyObject,
      c => for {
        messageId <- (c --\ "message_id").as[Int]
        from <- (c --\ "from").as[User]
        chat <- (c --\ "chat").as[Chat]
        text <- (c --\ "text").as[Option[String]]
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

case class User(id: Int, firstName: Option[String] = None, lastName: Option[String] = None, username: String)
object User {

  implicit def UserCodecJson: CodecJson[User] = CodecJson (
    (user: User) =>
      ("id" := user.id) ->:
        user.firstName.map("first_name" := _) ->?:
        user.lastName.map("last_name" := _) ->?:
        ("username" := user.username) ->:
        jEmptyObject,
    u => for {
      id <- (u --\ "id").as[Int]
      firstName <- (u --\ "first_name").as[Option[String]]
      lastName <- (u --\ "last_name").as[Option[String]]
      username <- (u --\ "username").as[String]
    } yield User(id, firstName, lastName, username)
  )
}

case class SendMessage(chatId: Int,
                       text: String,
                       parseMode: Option[String] = None,
                       disableWebPagePreview: Option[Boolean] = None,
                       disableNotification: Option[Boolean] = None,
                       replyToMessageId: Option[Int] = None,
                       replyMarkup: Option[ReplyKeyboardMarkup] = None)
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
  import argonaut._, Argonaut._

  private val baseUri = "https://api.telegram.org/bot180715621:AAFUc2sYIfo4FYww_01v3VGZXiB_WcPPL4k"
  private val getUpdatesUri = s"$baseUri/getUpdates"
  private val sendMessageUri = s"$baseUri/sendMessage"
  private val sendPhotoUri = s"$baseUri/sendPhoto"

  private implicit val system = ActorSystem("system")
  private implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(system))
  private val http = Http(system)

  import scala.concurrent.ExecutionContext.Implicits.global


  def getUpdates(updateId: Int = 0): Future[Option[List[Update]]] = {
    http.singleRequest(HttpRequest(uri = s"$getUpdatesUri?offset=$updateId"))
      .flatMap(_.entity.dataBytes.runFold(ByteString(""))(_ ++ _))
      .map(_.decodeString(StandardCharsets.UTF_8.toString))
      .map {
        response =>
          response.decodeValidation[Response].fold(
            {
              errorMessage =>
                logger.error(response + " " + errorMessage)
                None
            },
            r => Some(r.result))
      }
  }

  def sendMessage(message: SendMessage): Future[HttpResponse] = {
    http.singleRequest(HttpRequest(
      uri = sendMessageUri,
      method = HttpMethods.POST,
      entity = HttpEntity(MediaTypes.`application/json`, message.asJson.nospaces)
    ))
  }

  def sendPhoto(chatId: Int, photo: Array[Byte]): Future[HttpResponse] = {
    val entity = Multipart.FormData(
      BodyPart.Strict (
        "chat_id",
        HttpEntity.Strict(ContentType(MediaTypes.`text/plain`, HttpCharsets.`UTF-8`), ByteString(chatId.toString))
      ),
      BodyPart(
        "photo",
        HttpEntity(ContentType(MediaTypes.`image/png`), photo), Map("filename" -> "sample.png"))
    ).toEntity()
    http.singleRequest(HttpRequest(
      method = HttpMethods.POST,
      uri = sendPhotoUri,
      entity = entity
    ))
  }
}


