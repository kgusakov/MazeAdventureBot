import java.io.{File, FileInputStream}
import java.net.URL
import java.nio.file.{Files, Paths}
import java.util.UUID

import org.scalatest.FunSuite
import org.scalatest._
import Matchers._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok
import akka.http.scaladsl.model.Multipart.FormData.BodyPart
import akka.http.scaladsl.model._
import akka.stream.scaladsl.{FileIO, Source}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.util.ByteString
import com.maze.bot.telegram.api.{Response, SendMessage, TelegramApiClient}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}

import scala.concurrent.Await

/**
  * Created by kgusakov on 21.05.16.
  */
class MarshallingTest extends FunSuite with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global

  private val testJson =
    """
      |{"ok":true,"result":[
      |{"update_id":57150358,
      |"message":{"message_id":6,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov","type":"private"},"date":1463782167,"text":"sdf"}},
      |{"update_id":57150359,
      |"message":{"message_id":7,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov","type":"private"},"date":1463841796,"text":"asdf"}}]}
    """.stripMargin

  private val commandJson =
    """
      |{"ok":true,"result":[{"update_id":424915235,
      |"message":{"message_id":19,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov","type":"private"},"date":1465130988,"text":"\/help me","entities":[{"type":"bot_command","offset":0,"length":5}]}}]}
    """.stripMargin

  private val chatWithoutUserJson =
    """
      |{"ok":true,"result":[
      |{"update_id":327884173,
      | "message":
      |   {"message_id":111,
      |    "from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},
      |    "chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},
      |    "date":1465594073,
      |    "text":"\/new",
      |    "entities":[{"type":"bot_command","offset":0,"length":4}]}}]}
    """.stripMargin

  val failJson =
    """
      |{"ok":true,"result":[{"update_id":327884226,
      |"message":{"message_id":265,"from":{"id":91765916,"first_name":"Alexandra","username":"aabogacheva"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466326789,"text":"\/join","entities":[{"type":"bot_command","offset":0,"length":5}]}},{"update_id":327884227,
      |"message":{"message_id":266,"from":{"id":91765916,"first_name":"Alexandra","username":"aabogacheva"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466326806,"text":"\/join","entities":[{"type":"bot_command","offset":0,"length":5}]}},{"update_id":327884228,
      |"message":{"message_id":267,"from":{"id":91765916,"first_name":"Alexandra","username":"aabogacheva"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466326836,"text":"\/join@MaxeAdventureBot","entities":[{"type":"bot_command","offset":0,"length":22}]}},{"update_id":327884229,
      |"message":{"message_id":268,"from":{"id":91765916,"first_name":"Alexandra","username":"aabogacheva"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466326846,"text":"\/join@MazeAdventureBot","entities":[{"type":"bot_command","offset":0,"length":22}]}},{"update_id":327884230,
      |"message":{"message_id":269,"from":{"id":91765916,"first_name":"Alexandra","username":"aabogacheva"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466326895,"text":"\/join@MazeAdventureBot","entities":[{"type":"bot_command","offset":0,"length":22}]}},{"update_id":327884231,
      |"message":{"message_id":270,"from":{"id":91765916,"first_name":"Alexandra","username":"aabogacheva"},"chat":{"id":91765916,"first_name":"Alexandra","username":"aabogacheva","type":"private"},"date":1466326913,"text":"\/start","entities":[{"type":"bot_command","offset":0,"length":6}]}},{"update_id":327884232,
      |"message":{"message_id":271,"from":{"id":91765916,"first_name":"Alexandra","username":"aabogacheva"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466326919,"text":"\/join@MazeAdventureBot","entities":[{"type":"bot_command","offset":0,"length":22}]}},{"update_id":327884233,
      |"message":{"message_id":272,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466326942,"text":"\/join@MazeAdventureBot","entities":[{"type":"bot_command","offset":0,"length":22}]}}]}
    """.stripMargin

  val failJson1 =
    """
      |{"ok":true,"result":[{"update_id":327884239,
      |"edited_message":{"message_id":289,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466327699,"edit_date":1466327701,"text":"\/move@MazeAdventureBot up","entities":[{"type":"bot_command","offset":0,"length":22}]}},{"update_id":327884240,
      |"message":{"message_id":291,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466327708,"text":"\/move@MazeAdventureBot","entities":[{"type":"bot_command","offset":0,"length":22}]}},{"update_id":327884241,
      |"message":{"message_id":292,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466327713,"text":"\/move@MazeAdventureBot up","entities":[{"type":"bot_command","offset":0,"length":22}]}},{"update_id":327884242,
      |"message":{"message_id":293,"from":{"id":91765916,"first_name":"Alexandra","username":"aabogacheva"},"chat":{"id":-121062390,"title":"Chat Bot Test","type":"group"},"date":1466327720,"text":"\/move","entities":[{"type":"bot_command","offset":0,"length":5}]}}]}
    """.stripMargin

  val failJson2 =
    """
      |{"ok":true,"result":[{"update_id":327884361,
      |"message":{"message_id":533,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":-5240031,"title":"ex-opensoft-party","type":"group"},"date":1466354182,"new_chat_participant":{"id":180715621,"first_name":"MazeAdventureBot","username":"MazeAdventureBot"},"new_chat_member":{"id":180715621,"first_name":"MazeAdventureBot","username":"MazeAdventureBot"}}},{"update_id":327884362,
      |"message":{"message_id":534,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":-5240031,"title":"ex-opensoft-party","type":"group"},"date":1466354198,"text":"\/new@MazeAdventureBot","entities":[{"type":"bot_command","offset":0,"length":21}]}},{"update_id":327884363,
      |"message":{"message_id":535,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":-5240031,"title":"ex-opensoft-party","type":"group"},"date":1466354208,"text":"\/start","entities":[{"type":"bot_command","offset":0,"length":6}]}},{"update_id":327884364,
      |"message":{"message_id":536,"from":{"id":125504090,"first_name":"Kirill","last_name":"Gusakov","username":"kgusakov"},"chat":{"id":-5240031,"title":"ex-opensoft-party","type":"group"},"date":1466354210,"text":"\/new@MazeAdventureBot","entities":[{"type":"bot_command","offset":0,"length":21}]}}]}
    """.stripMargin

  test ("Unmarshalling of Updates") {
    import argonaut._, Argonaut._
    testJson.decodeValidation[Response].toOption.get.result.size should be (2)
  }

  test ("Unmarshalling of Updates with entities") {
    import argonaut._, Argonaut._
    commandJson.decodeValidation[Response].toOption.get.result(0).message.entities.get.head.`type` should be ("bot_command")
  }

  ignore ("getUpdates") {
    whenReady(TelegramApiClient.getUpdates()) { results =>
      results.get.size should be (2)
    }
  }

  test ("chatWithoutUserJson") {
    import argonaut._, Argonaut._
    println (chatWithoutUserJson.decodeEither[Response].leftSideValue)
    chatWithoutUserJson.decodeValidation[Response].isSuccess should be (true)
  }

  test ("failingJson") {
    import argonaut._, Argonaut._
    val result = failJson.decodeValidation[Response]
    println (failJson.decodeEither[Response].leftSideValue)
    failJson.decodeValidation[Response].isSuccess should be (true)
  }

  test ("failingJson1") {
    import argonaut._, Argonaut._
    val result = failJson1.decodeValidation[Response]
    println (failJson1.decodeEither[Response].leftSideValue)
    failJson1.decodeValidation[Response].isSuccess should be (true)
  }

  test ("failingJson2") {
    import argonaut._, Argonaut._
    val result = failJson2.decodeValidation[Response]
    println (failJson2.decodeEither[Response].leftSideValue)
    failJson2.decodeValidation[Response].isSuccess should be (true)
  }

//  ignore ("multipart") {
//    import argonaut._, Argonaut._
//    import scala.concurrent.duration._
//    val chatId = -121062390
//    val fileName = "/tmp/t.png"
//
//    val response = Await.result(TelegramApiClient.sendPhoto(chatId, new FileInputStream(fileName)), 5 seconds)
//    println(response.entity)
//  }

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(timeout =  Span(5, Seconds), interval = Span(500, Millis))
}
