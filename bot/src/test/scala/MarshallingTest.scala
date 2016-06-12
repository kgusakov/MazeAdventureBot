import java.net.URL

import com.stackmob.newman.response.{HttpResponse, HttpResponseCode}
import org.scalatest.FunSuite
import org.scalatest._
import Matchers._
import com.maze.bot.telegram.api.{Response, SendMessage, TelegramApiClient}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Millis, Seconds, Span}

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

  ignore ("echoTest")  {
    whenReady(TelegramApiClient.getUpdates()) {
      case Some(results) => results.foreach{u =>
        import argonaut._, Argonaut._
        if (!u.message.text.isEmpty)
          whenReady(TelegramApiClient.sendMessage(SendMessage(u.message.chat.id, u.message.text))) {
            case response: HttpResponse =>
              response.code should be (HttpResponseCode.Ok)
          }}
    }
  }

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(timeout =  Span(5, Seconds), interval = Span(500, Millis))
}
