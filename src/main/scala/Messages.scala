import akka.actor.ActorRef

abstract class NodeType
case object PropagateMaxType extends NodeType
case object PropagateMinType extends NodeType
case object AverageCountingType extends NodeType
case object UnknownType extends NodeType

case object AskValue
case class GiveValue(value: Double, to: NodeType = UnknownType)
case class MakeGrid(n: Int)
case class Broadcast(message: Any)
case class CommAction(command: String)
case class SetValue(node: String, value: Double)
case class GiveNeighbour(reference: ActorRef)