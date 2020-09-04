import akka.actor.ActorRef

import scala.concurrent.Future
import scala.reflect.ClassTag

case object AskValue
case object AskResult
case class GiveValue[T <: Node: ClassTag](value: Double)
case class GiveValuePropagateMax(value: Double)
case class GiveValuePropagateMin(value: Double)
case class GiveValueMinMax(min: Double, max: Double)
//case class MakeGrid(n: Int)
case class MakeNetwork[T <: Node](networkShape: String, params: Map[String, Any])(implicit val ct: ClassTag[T])
case class CommAction(command: String)
case class SetValue(node: String, value: Double)
case class GiveNeighbour(reference: ActorRef)
case class GiveACInterval(index: Int, interval: Array[Double])
case class GiveACTable(table: Array[Array[Double]])

//Evaluation
case class Acc[T](acc: T)
//case class Evaluate[T](frame: Int, acc: Acc[T], f: (Acc[T], Double) => Acc[T])
//case class Evaluation[T](frame: Int, actual: Future[Acc[T]], max: Future[Double], min: Future[Double], avg: Future[Double], acc: Acc[T], f: (Acc[T], Double) => Acc[T])
case class Evaluate(frame: Int)
case class Evaluation(frame: Int, actuals: Iterable[Future[Double]], results: Iterable[Future[Double]])
case class PlotGrid(frame: Int)

//Synchronization
case object AllReported
case object SingleStep
case object NetworkReady
case object AllReady
case object NodeReady

