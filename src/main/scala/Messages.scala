import akka.actor.ActorRef
import scala.reflect.ClassTag

case object AskValue
case class GiveValue[T <: Node: ClassTag](value: Double)
case class AutoValue[T <: Node: ClassTag](value: Double)
case class MakeGrid(n: Int)
case class MakeNetwork[T <: Node: ClassTag](networkShape: String, params: Map)
case class Broadcast(message: Any)
case class CommAction(command: String)
case class SetValue(node: String, value: Double)
case class GiveNeighbour(reference: ActorRef)
