import akka.actor._

class PropagateMax(diameter: Int) extends Node(diameter){

  var maxSeen: Double = 0
  def individualReceive: Receive = {
    case SingleStep =>
      neighs.foreach(_ ! GiveValuePropagateMax(maxSeen))
      logger.debug(s"SingleStep ran for ${self.path}: $value")
    case GiveValue(receivedValue) =>
      logger.debug(s"Received value $receivedValue")
      value = receivedValue
      maxSeen = receivedValue.max(maxSeen)
    case GiveValuePropagateMax(receivedValue) =>
      logger.debug(s"GiveValuePropagateMax receivedValue: $receivedValue")
      maxSeen = receivedValue.max(maxSeen)
      synchronizationCheck(sender)
  }
  def result(): Double = maxSeen
}
