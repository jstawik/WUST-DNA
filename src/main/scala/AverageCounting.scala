import akka.actor._
import akka.pattern.ask

import scala.concurrent.Await
import scala.util.Random

class AverageCounting(network: ActorRef, diameter: Int, intervals: Int, variables: Int) extends Node(network){
  val maxNode: ActorRef =  context.actorOf(Props(classOf[PropagateMax], self), "maxModule")
  val minNode: ActorRef =  context.actorOf(Props(classOf[PropagateMin], self), "minModule")
  val data: Array[Array[Double]] = Array.fill[Double](intervals, variables)(Double.MaxValue)
  var maxSeen: Double = 0
  var minSeen: Double = 0
  var range: Double = 0
  var interval: Double = 0
  var individualInterval: Int = 0
  def individualReceive: Receive = {
    case GiveNeighbour(neighbour) =>
      neighs = neighs :+ neighbour
      maxNode ! GiveNeighbour(neighbour)
      minNode ! GiveNeighbour(neighbour)
    case message: GiveValuePropagateMax => maxNode ! message
    case message: GiveValuePropagateMin => minNode ! message
    case GiveValue(receivedValue) =>
      value = receivedValue
      minNode ! GiveValue(receivedValue)
      maxNode ! GiveValue(receivedValue)
    case CommAction("networkReady") => networkReady()
    case GiveACInterval(index, interval) =>
      val old_row = for{row <- data} yield row(index)
      val temp_row = old_row
      for(i <- interval.indices) temp_row(i) = temp_row(i).min(interval(i))
      if(!old_row.sameElements(temp_row)){
        for(i <- temp_row.indices) data(index)(i) = temp_row(i)
        network ! Broadcast(GiveACInterval(index, temp_row))
      }
    case CommAction("reportResults") => sender() ! reportResults()

  }
  def networkReady(): Unit = {
    logger.debug("networkReady received, running min and max")
    maxNode ! CommAction("autoPropagate")
    val maxFuture = maxNode ? AskValue
    maxSeen = Await.result(maxFuture, timeout.duration).asInstanceOf[Double]
    minNode ! CommAction("autoPropagate")
    val minFuture = minNode ? AskValue
    minSeen = Await.result(minFuture, timeout.duration).asInstanceOf[Double]
    range = maxSeen - minSeen
    interval = range/intervals
    individualInterval = ((value-minSeen)/interval).toInt
    for(variable <- 0 until variables){
      data(individualInterval)(variable) = -Math.log(Random.nextDouble())
    }
    network ! Broadcast(GiveACInterval(individualInterval, for{row <- data} yield row(individualInterval)))
  }
  def reportResults(): Double = {
    val S: Array[Double] = data.map(_.sum)
    val H: Array[Double] = S.map(a => if (a > 0) (variables-1)/a else 0)
    val S1 = (for(i <- H.indices) yield minSeen + interval*(i - 1/2)*H(i)).sum
    val S2 = H.sum
    S1/S2
  }
}
