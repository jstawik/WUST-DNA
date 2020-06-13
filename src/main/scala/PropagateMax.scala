import akka.actor._

case class GiveValuePropagateMax(value: GiveValue[PropagateMax])

class PropagateMax(network: ActorRef) extends Node(network){
  var maxSeen: Double = 0
  def propagateMaxReceive: Receive = {
    case CommAction("broadcastValue") => neighs.foreach(_ ! GiveValue[PropagateMax](maxSeen))
      logger.debug(s"broadcastValue ran: $value")
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
