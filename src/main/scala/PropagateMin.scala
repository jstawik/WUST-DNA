import akka.actor._

class PropagateMin(network: ActorRef) extends Node(network){
  /* REFACTOR: Creating a separate Propagate class for each function (now it's min and max) results in code duplication.
     This could be rewritten to accept any monoid as an argument to implement any such function
   */
  var minSeen: Double = 0
  def propagateMaxReceive: Receive = {
    case CommAction("broadcastValue") => neighs.foreach(_ ! GiveValue(minSeen, PropagateMaxType))
      logger.debug(s"broadcastValue ran: $value")
    case GiveValue(receivedValue, _) =>
      value = receivedValue
      minSeen = receivedValue.min(minSeen)
    case AskValue =>
      sender() ! minSeen
  }
  def broadcastTemperature(): Unit = {
  }
  def receive: Receive = propagateMaxReceive orElse commonReceive
}
