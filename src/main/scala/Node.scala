import akka.actor._
import akka.util.Timeout
import com.typesafe.scalalogging.Logger
import akka.pattern.ask

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps


abstract class Node(val network: ActorRef) extends Actor with ActorLogging{
  var value: Double = 0.0
  var sent: Int = 0
  def commonReceive: Receive = {
    case CommAction("askValue") => askValue()
    case GiveValue(receivedValue) =>
      logger.debug(s"Received GiveValue: $receivedValue")
      value = receivedValue
    case AskValue =>
      sender() ! value
      sent += 1
    case _ => logger.error(s"Unhandled message from ${sender().path.name}")
  }
  implicit val timeout: Timeout = Timeout(5 seconds)
  val logger: Logger = Logger(s"${self.path.name}")
  def askValue(): Unit = {
    val future = network ? AskValue
    value = Await.result(future, timeout.duration).asInstanceOf[Double]
  }
}



