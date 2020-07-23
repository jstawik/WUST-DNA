import akka.actor.ActorRef
import scala.reflect.ClassTag

case object AskValue
case object AskResult
case class GiveValue[T <: Node: ClassTag](value: Double)
case class AutoValue[T <: Node: ClassTag](value: Double)
case class MakeGrid(n: Int)
case class MakeNetwork[T <: Node](networkShape: String, params: Map[String, Any])(implicit val ct: ClassTag[T])
case class Broadcast(message: Any)
case class CommAction(command: String)
case class SetValue(node: String, value: Double)
case class GiveNeighbour(reference: ActorRef)
case class GiveACInterval(index: Int, interval: Array[Double])
case class Evaluate(f: (Double, Double) => Double)
case class Evaluation(actual: Double, max: Double, min: Double, avg: Double)