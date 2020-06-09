import akka.actor._

class PropagateMax(network: ActorRef) extends Node(network){
  var maxSeen: Double = 0
  def propagateMaxReceive: Receive = {
    case CommAction("broadcastValue") => neighs.foreach(_ ! MaxValue(maxSeen))
      logger.debug(s"broadcastValue ran: $value")
    case MaxValue(receivedValue) =>
      logger.debug(s"Received MaxValue: $receivedValue")
      maxSeen = receivedValue.max(maxSeen)
    case GiveValue(receivedValue) =>
      value = receivedValue
      maxSeen = receivedValue.max(maxSeen)
    case AskValue =>
      sender() ! maxSeen
  }
  def broadcastTemperature(): Unit = {
  }
  def receive: Receive = propagateMaxReceive orElse commonReceive
}
