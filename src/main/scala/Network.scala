import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import scala.concurrent.Await
import scala.util.Random
import scala.reflect._

import scala.concurrent.duration._
import scala.language.postfixOps

class Network extends Actor{
  implicit val timeout: Timeout = Timeout(5 seconds)
  private val value = () => Random.nextDouble() * 5
  val logger: Logger = Logger(s"${self.path.name}")

  var nodes = Map.empty[String, ActorRef]

  def receive: Receive = {
    case AskValue => sender() ! value()
    case MakeGrid(n) => makeGrid[PropagateMax](n)
    case f @ MakeNetwork(networkShape, params) => networkShape match {
      case "grid" => makeGrid((params getOrElse ("side", 100)).asInstanceOf[Int])(f.ct)
      case "line" => makeLine((params getOrElse ("n", 100)).asInstanceOf[Int])(f.ct)
      case "gridClique" => makeGridClique((params getOrElse ("side", 100)).asInstanceOf[Int], (params getOrElse("csize", 10)).asInstanceOf[Int])(f.ct)
      case _ => logger.error(s"Unhandled message from ${sender().path.name}" + s" unknown networkShape: $networkShape")
    }
    case CommAction("maxPropagation") => nodes.values.map(n => n ! CommAction("broadcastValue"))
    case CommAction("plotGrid") => Plotter.makeHeatMap(gridView(), "Grid View")
    case SetValue(node, value) => nodes(node) ! GiveValue(value)
    case _ => logger.error(s"Unhandled message from ${sender().path.name}")
  }

  /**
   * Adds a Grid-Clique graph of nodes. First 4 nodes in clique are the gateways, mapping being 0123 <-> NWES
   * @param side Side of the macro grid
   * @param csize Count of nodes in a clique
   * @tparam T Type of node
   */
  def makeGridClique[T <: Node: ClassTag](side: Int, csize: Int): Unit ={
    val coord = (x: Int, y:Int, i:Int) => s"node_${x}_${y}_$i"
    for(x <- 0 until side){
      for(y <- 0 until side){
        for(c <- 0 until csize){
          val newNode = context.actorOf(Props(classTag[T].runtimeClass, self), coord(x,y,c))
          nodes = nodes + (coord(x,y,c) -> newNode)
        }
      }
    }
    for(x <- 0 until side){
      for(y <- 0 until side){
        for(c <- 0 until csize){
          val newNeighs = (i: Int) => for{j <- 0 until csize if j != i} yield j
          for(i <- newNeighs(c)) nodes(coord(x, y, c)) ! GiveNeighbour(nodes(coord(x,y,i)))
        }
        if(x-1 >= 0) nodes(coord(x,y,0)) ! GiveNeighbour(nodes(coord(x-1,y,3)))
        if(x+1 < side) nodes(coord(x,y,3)) ! GiveNeighbour(nodes(coord(x+1,y,0)))
        if(y-1 >= 0) nodes(coord(x,y,1)) ! GiveNeighbour(nodes(coord(x,y-1,2)))
        if(y-1 < side) nodes(coord(x,y,2)) ! GiveNeighbour(nodes(coord(x,y+1,1)))

      }
    }
    logger.debug("makeGridClique ran")
    logger.debug(s"nodes is now: $nodes")
    scrambleValues[T]()
    nodes.foreach(_._2 ! CommAction("networkReady"))
  }

  def makeGrid[T <: Node: ClassTag](side: Int): Unit = {
    val coord = (x: Int, y:Int) => s"node_${x}_$y"
    for(x <- 0 until side){
      for(y <- 0 until side){
        val newNode = context.actorOf(Props(classTag[T].runtimeClass, self), coord(x,y))
        nodes = nodes + (coord(x,y) -> newNode)
      }
    }
    for(x <- 0 until side){
      for(y <- 0 until side){
        val newNeighs = (i: Int) => List(i-1, i+1).filter(_>=0).filter(_<side)
        for(i <- newNeighs(x)) nodes(coord(x, y)) ! GiveNeighbour(nodes(coord(i, y)))
        for(i <- newNeighs(y)) nodes(coord(x, y)) ! GiveNeighbour(nodes(coord(x, i)))
      }
    }
    logger.debug("makeGrid ran")
    logger.debug(s"nodes is now: $nodes")
    scrambleValues[T]()
    nodes.foreach(_._2 ! CommAction("networkReady"))
  }
  def makeLine[T <: Node: ClassTag](n: Int): Unit = {
    val coord = (i: Int) => s"node_$i"
    for(i <- 0 until n){
      val newNode = context.actorOf(Props(classTag[T].runtimeClass, self), coord(i))
      nodes = nodes + (coord(i) -> newNode)
    }
    for(i <- 0 until n){
      val newNeighs = (i: Int) => List(i-1, i+1).filter(_>=0).filter(_<n)
      for(j <- newNeighs(i)) nodes(coord(i)) ! GiveNeighbour(nodes(coord(j)))
    }
    logger.debug("makeLine ran")
    logger.debug(s"nodes is now: $nodes")
    scrambleValues[T]()
    nodes.foreach(_._2 ! CommAction("networkReady"))
  }
  def scrambleValues[T <: Node: ClassTag](): Unit = nodes.values.foreach(_ ! GiveValue[T](value()))

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

