import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask

import scala.collection.mutable
import scala.concurrent.Await
import scala.reflect._
import scala.util.Random

class Network extends Actor with ActorDefaults{
  private val value = () => Random.nextDouble() * 5
  var nodes = Map.empty[String, ActorRef]
  var nodesReported = Set.empty[ActorRef]
  var nodesReady = Set.empty[ActorRef]

  def receive: Receive = {
    case AskValue => sender() ! value()
    case f @ MakeNetwork(networkShape, params) =>
      networkShape match {
        case "grid" => makeGrid(params.getOrElse("side", 100).asInstanceOf[Int])(f.ct)
        case "line" => makeLine(params.getOrElse("n", 100).asInstanceOf[Int])(f.ct)
        case "gridClique" => makeGridClique(params.getOrElse("side", 100).asInstanceOf[Int], params.getOrElse("csize", 10).asInstanceOf[Int])(f.ct)
        case "randomGeometric" => makeRandomGeometric(params.getOrElse("count", 100).asInstanceOf[Int], params.getOrElse("radius", 0.1).asInstanceOf[Double])(f.ct)
        case _ => logger.error(s"Unhandled message from ${sender().path.name}" + s" unknown networkShape: $networkShape")
      }
      logger.debug(s"Network created, ${self.path} about to respond to ${sender().path} with NetworkReady")
      sender() ! NetworkReady
    case PlotGrid(frame) => Plotter.makeHeatMap(frame, gridView(), "Grid View")
    case Evaluate(frame, acc, f) =>
      val actual = nodes.map(rec => askValue(rec._2)).foldLeft(acc)(f)
      val res =  nodes.map(rec => askData(rec._2, AskResult))
      sender() ! Evaluation(frame, actual, res.reduce(_.max(_)), res.reduce(_.min(_)), res.sum/res.size)
    case SetValue(node, value) => nodes(node) ! GiveValue(value)
    case SingleStep =>
      logger.debug(s"SingleStep received by ${self.path} propagating to $nodes")
      nodes.values.map(n => n ! SingleStep)
    case AllReported =>
      nodesReported += sender()
      if(nodesReported.size == nodes.size) {
        nodesReported = Set.empty[ActorRef]
        context.parent ! AllReported
        logger.debug(s"AllReported sent to ${context.parent}")
      }
    case NodeReady =>
      nodesReady += sender()
      if(nodesReady.size == nodes.size){
        nodesReady = Set.empty[ActorRef]
        context.parent ! AllReady
        logger.debug(s"AllReady sent to ${context.parent}")
      }
    case m @ _ => logger.error(s"Unhandled message from ${sender().path}: $m")
  }


  /**
   * Adds a Grid-Clique graph of nodes. First 4 nodes in clique are the gateways, mapping being 0123 <-> NWES and are required
   * @param side Side of the macro grid
   * @param csize Count of nodes in a clique
   * @tparam T Type of node
   */
  def makeGridClique[T <: Node: ClassTag](side: Int, csize: Int): Unit ={
    val diameter = 0.max(4 * side - 5)
    val coord = (x: Int, y:Int, i:Int) => s"node_${x}_${y}_$i"
    for(x <- 0 until side){
      for(y <- 0 until side){
        for(c <- 0 until csize){
          val newNode = context.actorOf(Props(classTag[T].runtimeClass, diameter), coord(x,y,c))
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
    establishNetwork("makeGridClique")
  }

  /**
   * Create a square grid of nodes
   * @param side Side of grid
   * @tparam T Type of nodes
   */
  def makeGrid[T <: Node: ClassTag](side: Int): Unit = {
    val diameter =2 * side - 2
    val coord = (x: Int, y:Int) => s"node_${x}_$y"
    for(x <- 0 until side){
      for(y <- 0 until side){
        val newNode = context.actorOf(Props(classTag[T].runtimeClass, diameter), coord(x,y))
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
    establishNetwork("makeGrid")
  }

  /**
   * Create a line of nodes
   * @param n Length of nodes
   * @tparam T Type of nodes
   */
  def makeLine[T <: Node: ClassTag](n: Int): Unit = {
    val diameter = 0.max(n - 1)
    val coord = (i: Int) => s"node_$i"
    for(i <- 0 until n){
      val newNode = context.actorOf(Props(classTag[T].runtimeClass, diameter), coord(i))
      nodes = nodes + (coord(i) -> newNode)
    }
    for(i <- 0 until n){
      val newNeighs = (i: Int) => List(i-1, i+1).filter(_>=0).filter(_<n)
      for(j <- newNeighs(i)) nodes(coord(i)) ! GiveNeighbour(nodes(coord(j)))
    }
    establishNetwork("makeLine")
  }

  /**
   * Create a random Geometric graph
   * @param count Node count
   * @param radius Maximum connection length
   * @tparam T Type of node
   */
  def makeRandomGeometric[T <: Node: ClassTag](count: Int, radius: Double): Unit = {
    val diameter = count //TODO: No, no it isn't.
    val metric = (x0: Double, y0: Double, x1: Double, y1: Double) => Math.sqrt(Math.pow(x0-x1, 2) + Math.pow(y0-y1, 2)) <= radius
    val coord = (i: (Int, Double, Double)) => s"node_${i._1}"
    val list = for{i <- 0 until count} yield (i, Random.nextDouble(), Random.nextDouble())
    for(i <- list) nodes = nodes + (coord(i) -> context.actorOf(Props(classTag[T].runtimeClass, diameter), coord(i)))
    for(from <- list){
      for(to <- list if to != from) if(metric(from._1, from._2, to._1, to._2)) nodes(coord(from)) ! GiveNeighbour(nodes(coord(to)))
    }
    establishNetwork("makeRandomGeometric")
  }

  /**
   * Create a `n`-regular graph. The graph will not be random. This means, each node will be as if set on a ring and
   * connected to `n` nearest neighbours for even `n`s. For odd each node will also be connected to the one on the
   * opposite side.
   * @param count Node count
   * @param n Each node's degree
   * @tparam T Type of node
   */
  def makeNRegular[T <: Node: ClassTag](count: Int, n: Int): Unit = {
    val diameter = (count.toFloat/n).ceil
    if ((n * count % 2) != 0) throw new IllegalArgumentException("`count` * `n` can't be odd")
    val coord = (i: Int) => s"node_$i"
    for (i <- 0 until n) nodes = nodes + (coord(i) -> context.actorOf(Props(classTag[T].runtimeClass, diameter), coord(i)))
    for (i <- 0 until n) {
      for (j <- 1 to n/2){
        nodes(coord(j)) ! GiveNeighbour(nodes(coord(i+j)))
        nodes(coord(i+j)) ! GiveNeighbour(nodes(coord(i)))
      }
      if(n % 2 != 0 ){
        nodes(coord(i)) ! GiveNeighbour(nodes(coord((i+n/2)%n)))
        nodes(coord((i+n/2)%n)) ! GiveNeighbour(nodes(coord(i)))
      }
    }
    establishNetwork("makeNRegular")
  }

    def establishNetwork[T <: Node: ClassTag](name: String): Unit = {
      logger.debug(s"$name ran. Nodes is now: $nodes")
      nodes.values.foreach(_ ! GiveValue[T](value()))
      //nodes.foreach(_._2 ! NetworkReady)
    }
  /**
   * Generate a matrix of grid's values
   * @return 2D matrix in which each field is a value of node with respective coordinates.
   */
  def gridView(): Seq[Seq[Double]] = {
    var nodeView = Seq.empty[(Int, Int, Double)]
    for(node <- nodes){
      val x = node._1.split("_")(1).toInt
      val y = node._1.split("_")(2).toInt
      val value = askData(node._2, AskResult)
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
  def askData(node: ActorRef, message: Product): Double = {
    val future = node ? message
    Await.result(future, timeout.duration).asInstanceOf[Double]
  }
  def askValue(node: ActorRef): Double = askData(node, AskValue)
}

