import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask

import scala.collection.mutable
import scala.concurrent.{Await, Future}
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
        case "grid" => makeGrid(params.getOrElse("side_a", 1).asInstanceOf[Int], params.getOrElse("side_b", 1).asInstanceOf[Int])(f.ct)
        case "gridClique" => makeGridClique(params.getOrElse("side", 100).asInstanceOf[Int], params.getOrElse("csize", 10).asInstanceOf[Int])(f.ct)
        case "randomGeometric" => makeRandomGeometric(params.getOrElse("count", 100).asInstanceOf[Int], params.getOrElse("radius", 0.1).asInstanceOf[Double])(f.ct)
        case _ => logger.error(s"Unhandled message from ${sender().path.name}" + s" unknown networkShape: $networkShape")
      }
      logger.debug(s"Network created, ${self.path} about to respond to ${sender().path} with NetworkReady")
      sender() ! NetworkReady
    case PlotGrid(frame) => Plotter.makeHeatMap(frame, gridView(), "Grid View")
    case Evaluate(frame) =>
      val actuals: Iterable[Future[Double]] = nodes.values.map(ask(_, AskValue).mapTo[Double])
      val results: Iterable[Future[Double]] =  nodes.values.map(ask(_, AskResult).mapTo[Double])
      sender() ! Evaluation(frame, actuals, results)
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
   * @param side_a Horizontal side of grid
   * @param side_b Vertical side of grid
   * @tparam T Type of nodes
   */
  def makeGrid[T <: Node: ClassTag](side_a: Int, side_b: Int): Unit = {
    val diameter = side_b + side_a - 2
    val coord = (x: Int, y:Int) => s"node_${x}_$y"
    for(x <- 0 until side_a){
      for(y <- 0 until side_b){
        val newNode = context.actorOf(Props(classTag[T].runtimeClass, diameter), coord(x,y))
        nodes = nodes + (coord(x,y) -> newNode)
      }
    }
    for(x <- 0 until side_a){
      for(y <- 0 until side_b){
        val newNeighs = (x: Int, y: Int) => for { a <- Seq(x-1, x+1) if a >= 0 && a < side_a
                                                  b <- Seq(y-1, y+1) if b >= 0 && b < side_b
                                            } yield (a, b)
        for(i <- newNeighs(x, y)) nodes(coord(x, y)) ! GiveNeighbour(nodes(coord(i._1, i._2)))
      }
    }
    establishNetwork("makeGrid")
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
}

