import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import scala.concurrent.Await
import scala.util.Random

import scala.concurrent.duration._
import scala.language.postfixOps

class Network extends Actor{
  implicit val timeout: Timeout = Timeout(5 seconds)
  val r: Random = Random
  val logger: Logger = Logger(s"${self.path.name}")
  val nodes = mutable.Map.empty[String, ActorRef]
  val neighs = mutable.Map.empty[String, mutable.Set[ActorRef]]

  def receive: Receive = {
    case AskValue => sender() ! r.nextDouble * 5
    case MakeGrid(n) => makeGrid(n)
    case Broadcast(MaxValue(temperature)) => neighs(sender().path.name).map(_ ! MaxValue(temperature))
    case CommAction("maxPropagation") => nodes.values.map(n => n ! CommAction("broadcastTemperature"))
    case CommAction("plotGrid") => Plotter.makeHeatMap(gridView(), "Grid View")
    case SetValue(node, value) => nodes(node) ! GiveValue(value)
    case _ => logger.error(s"Unhandled message from ${sender().path.name}")
  }

  def makeGrid(side: Int): Unit = {
    val coord = (x: Int, y:Int) => s"node_${x}_$y"
    for(x <- 0 until side){
      for(y <- 0 until side){
        val newNode = context.actorOf(Props(classOf[PropagateMax], self, r.nextDouble*5), coord(x,y))
        nodes += (coord(x,y) -> newNode)
        neighs += (coord(x,y) -> mutable.Set.empty)
      }
    }
    for(x <- 0 until side){
      for(y <- 0 until side){
        val newNeighs = (i: Int) => List(i-1, i+1).filter(_>=0).filter(_<side)
        for(i <- newNeighs(x)) neighs(coord(x,y)) += nodes(coord(i,y))
        for(i <- newNeighs(y)) neighs(coord(x,y)) += nodes(coord(x,i))
      }
    }
    logger.debug("makeGrid ran")
    logger.debug(s"nodes is now: $nodes")
    logger.debug(s"neighs is now: $neighs")
  }

  def gridView(): Seq[Seq[Double]] = {
    var nodeView = Seq.empty[(Int, Int, Double)]
    for(node <- nodes){
      val x = node._1.split("_")(1).toInt
      val y = node._1.split("_")(2).toInt
      val future = node._2 ? AskValue
      val value = Await.result(future, timeout.duration).asInstanceOf[Double]
      nodeView = nodeView.appended(x, y, value)
    }
    val n = nodeView.distinctBy(_._1).size
    val gridView = Seq.fill(n)(mutable.Seq.empty[Double].padTo(n, 0.0))
    for(node <- nodeView){
      gridView(node._1)(node._2) = node._3
    }
    logger.debug(s"gridView about to return: $gridView")
    gridView.map(_.toSeq)
  }
}

