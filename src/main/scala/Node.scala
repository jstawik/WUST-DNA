import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.scalalogging.Logger

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps


abstract class Node(val network: ActorRef) extends Actor with ActorLogging{
  var value: Double = 0.0
  var sent: Int = 0
  var neighs = Seq.empty[ActorRef]
  def individualReceive: Receive
  def commonReceive: Receive = {
    case GiveNeighbour(neighbour) => neighs = neighs :+ neighbour
    case CommAction("askValue") => askValue()
    case GiveValue(receivedValue) =>
      logger.debug(s"Received GiveValue: $receivedValue")
      value = receivedValue
    case AskValue => sender() ! value
    case AskResult => sender() ! result()
    case CommAction("networkReady") =>
    case _ => logger.error(s"Unhandled message from ${sender().path.name}")
  }
  def receive: Receive = individualReceive orElse commonReceive
  implicit val timeout: Timeout = Timeout(5 seconds)
  val logger: Logger = Logger(s"${self.path.name}")
  def askValue(): Unit = {
    val future = network ? AskValue
    value = Await.result(future, timeout.duration).asInstanceOf[Double]
  }
  def result(): Double
}



