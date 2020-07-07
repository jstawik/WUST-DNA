import akka.actor._

class AverageCounting(network: ActorRef) extends Node(network){
  val maxNode: ActorRef =  context.actorOf(Props(classOf[PropagateMax], self), "maxModule")
  val minNode: ActorRef =  context.actorOf(Props(classOf[PropagateMin], self), "minModule")
  override def individualReceive: Receive = {
    case GiveNeighbour(neighbour) => neighs = neighs :+ neighbour
      maxNode ! GiveNeighbour(neighbour)
      minNode ! GiveNeighbour(neighbour)
    case message: GiveValuePropagateMax => maxNode ! message
    case message: GiveValuePropagateMin => minNode ! message
    case GiveValue(receivedValue) =>
      value = receivedValue
      minNode ! GiveValue(receivedValue)
      maxNode ! GiveValue(receivedValue)
    case CommAction("networkReady") => networkReady()
  }
  def networkReady(): Unit = {
    logger.debug("networkReady received, running min and max")
    maxNode ! CommAction("autoPropagate")
    minNode ! CommAction("autoPropagate")
  }
}
