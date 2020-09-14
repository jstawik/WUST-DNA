import akka.actor._

import scala.collection.mutable
import scala.util.Random

abstract class Node(val diameter: Int) extends ActorDefaults{
  logger.debug(s"Created as instance of ${this.getClass.getName}")
  var value: Double = 0.0
  var sent: Int = 0
  var neighs = Seq.empty[ActorRef]
  val neighsReported = mutable.Set.empty[ActorRef]
  def individualReceive: Receive
  def commonReceive: Receive = {
    case GiveNeighbour(neighbour) =>
      neighs = neighs :+ neighbour
    case CommAction("askValue") => askValue()
    case GiveValue(receivedValue) =>
      logger.debug(s"Received GiveValue: $receivedValue")
      value = receivedValue
    case AskValue => sender() ! value
    case AskResult => sender() ! result()
    case NetworkReady =>
    case m @ _ => logger.error(s"Unhandled message from ${sender().path}: $m")
  }
  def receive: Receive = individualReceive orElse commonReceive
  def askValue(): Unit = {
    //val future = network ? AskValue
    //value = Await.result(future, timeout.duration).asInstanceOf[Double]
    value = Random.nextDouble() * 5 //TODO: probably a better way to take load off Network
  }
  def result(): Double
  def synchronizationCheck(sender: ActorRef): Unit = {
    neighsReported += sender
    if(neighsReported.size == neighs.size){
      neighsReported.clear()
      context.parent ! AllReported
      logger.debug(s"AllReported sent by ${self.path}")
    }
    //else {     //  logger.debug(s"Nodes reported: $neighsReported out of $neighs")
    //}
  }
}



