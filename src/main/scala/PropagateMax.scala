import akka.actor._

class PropagateMax(network: ActorRef) extends Node(network){
  var maxSeen: Double = 0
  def propagateMaxReceive: Receive = {
    case CommAction("broadcastValue") => neighs.foreach(_ ! GiveValue(maxSeen, PropagateMaxType))
      logger.debug(s"broadcastValue ran: $value")
    case GiveValue(receivedValue, _) =>
      value = receivedValue
      maxSeen = receivedValue.max(maxSeen)
    case AskValue =>
      sender() ! maxSeen
  }
  def broadcastTemperature(): Unit = {
  }
  def receive: Receive = propagateMaxReceive orElse commonReceive
}
