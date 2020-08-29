import akka.actor._

class PropagateMin(diameter: Int) extends Node(diameter){
  /* REFACTOR: Creating a separate Propagate class for each function (now it's min and max) results in code duplication.
     This could be rewritten to accept any monoid as an argument to implement any such function
   */
  var minSeen: Double = Double.MaxValue
  def individualReceive: Receive = {
    case SingleStep =>
      neighs.foreach(_ ! GiveValuePropagateMin(minSeen))
      logger.debug(s"SingleStep ran for ${self.path}: $value")
    case GiveValue(receivedValue) =>
      logger.debug(s"Received value $receivedValue")
      value = receivedValue
      minSeen = receivedValue.min(minSeen)
    case GiveValuePropagateMin(receivedValue) =>
      logger.debug(s"GiveValuePropagateMin receivedValue: $receivedValue")
      minSeen = receivedValue.min(minSeen)
      synchronizationCheck(sender())
  }
  def result(): Double = minSeen
}
