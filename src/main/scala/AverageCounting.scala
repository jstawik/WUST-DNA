import akka.actor._

class AverageCounting(network: ActorRef, var data: Double) extends Node(network){
  val maxNode: ActorRef =  context.actorOf(Props(classOf[PropagateMax], self), "maxModule")
  val minNode: ActorRef =  context.actorOf(Props(classOf[PropagateMin], self), "minModule")
  def averageCountingReceive : Receive = {
    case GiveNeighbour(neighbour) => neighs = neighs :+ neighbour
      maxNode ! GiveNeighbour(neighbour)
      minNode ! GiveNeighbour(neighbour)
    case message: GiveValuePropagateMax => maxNode ! message
    case message: GiveValuePropagateMin => minNode ! message
    case _ => logger.error(s"Unhandled message from ${sender().path.name}")
  }
  override def receive: Receive = averageCountingReceive orElse commonReceive
}
