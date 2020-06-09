import akka.actor.ActorRef

case object AskValue
case class GiveValue(value: Double)
case class MaxValue(value: Double)
case class MakeGrid(n: Int)
case class Broadcast(message: Any)
case class CommAction(command: String)
case class SetValue(node: String, value: Double)
case class GiveNeighbour(reference: ActorRef)