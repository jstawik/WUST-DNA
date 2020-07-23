import akka.actor._

case class GiveValuePropagateMin(value: GiveValue[PropagateMin])

class PropagateMin(network: ActorRef) extends Node(network){
  /* REFACTOR: Creating a separate Propagate class for each function (now it's min and max) results in code duplication.
     This could be rewritten to accept any monoid as an argument to implement any such function
   */
  var minSeen: Double = 0
  def individualReceive : Receive = {
    case CommAction("broadcastValue") => neighs.foreach(_ ! GiveValue[PropagateMin](minSeen))
      logger.debug(s"broadcastValue ran: $value")
    case CommAction("autoPropagate") => neighs.foreach(_ ! AutoValue[PropagateMin](minSeen))
    case AutoValue(receivedValue) =>
      if(receivedValue > minSeen){
        minSeen = receivedValue
        neighs.foreach(_ ! AutoValue[PropagateMin](minSeen))
      }
    case GiveValue(receivedValue) =>
      value = receivedValue
      minSeen = receivedValue.min(minSeen)
  }
  def result(): Double = minSeen
}
