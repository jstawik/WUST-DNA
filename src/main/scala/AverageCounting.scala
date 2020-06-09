import akka.actor.ActorRef

class AverageCounting(network: ActorRef, var data: Double) extends Node(network){
  def receive: Receive = {
    case _ => logger.error(s"Unhandled message from ${sender().path.name}")
  }
}
