import akka.actor._

class PropagateMax(network: ActorRef, var maxSeen: Double) extends Node(network){
  def propagateMaxReceive: Receive = {
    case CommAction("broadcastTemperature") =>
      network ! Broadcast(MaxValue(value))
      logger.debug(s"broadcastTemperature ran: $value")
    case MaxValue(receivedValue) =>
      logger.debug(s"Received MaxValue: $receivedValue")
      maxSeen = receivedValue.max(maxSeen)
  }
  def broadcastTemperature(): Unit = {
  }
  def receive: Receive = propagateMaxReceive orElse commonReceive
}
