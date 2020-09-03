import akka.actor._

import scala.util.Random

class AverageCounting(diameter: Int) extends Node(diameter){
  var setupIterations = 0
  var setupReported = Set.empty[ActorRef]
  var setupPhase = true

  val intervals: Int = 20
  val variables: Int = 10
  val data: Array[Array[Double]] = Array.fill[Double](intervals, variables)(Double.MaxValue)
  var maxSeen: Double = 0
  var minSeen: Double = Double.MaxValue
  var range: Double = 0
  var intervalWidth: Double = 0
  var individualInterval: Int = 0

  def individualReceive: Receive = {
    case GiveNeighbour(neighbour) =>
      neighs = neighs :+ neighbour
    case GiveValue(receivedValue) =>
      logger.debug(s"Received value $receivedValue")
      value = receivedValue
      minSeen = receivedValue.min(minSeen)
      maxSeen = receivedValue.max(maxSeen)
    case SingleStep =>
      if(setupPhase){
        neighs.foreach(_ ! GiveValueMinMax(minSeen, maxSeen))
        logger.debug(s"SingleStep setup phase ran for ${self.path}: $minSeen, $maxSeen")
      }
      else {
        neighs.foreach(_ ! GiveACTable(data))
        //for {i <- data.indices} neighs.foreach(_ ! GiveACInterval(i, data(i)))
        logger.debug(s"SingleStep main phase ran for ${self.path}")
      }
    case GiveACTable(table) =>
      for(row <- table.indices){
        val currentRow = data(row)
        val newRow = table(row)
        if(!currentRow.sameElements(newRow)) {
          val temp_row = currentRow
          for (cell <- temp_row.indices) temp_row(cell) = temp_row(cell).min(newRow(cell))
          data(row) = temp_row
        }
      }
      synchronizationCheck(sender)
//    case GiveACInterval(index, interval) =>
//      val old_row = data(index)
//      if(!old_row.sameElements(interval)){
//        val temp_row = old_row
//        for(i <- interval.indices) temp_row(i) = temp_row(i).min(interval(i))
//        data(index) = temp_row
//      }
//      synchronizationCheck(sender())
    case GiveValueMinMax(min, max) =>
      logger.debug(s"GiveValueMinMax min, max: $min, $max")
      minSeen = min.min(minSeen)
      maxSeen = max.max(maxSeen)
      neighsReported += sender
      if(neighsReported.size == neighs.size){
        neighsReported = Set.empty[ActorRef]
        logger.debug(s"AllReported sent by ${self.path}")
        setupIterations += 1
        if(setupIterations >= diameter){
          setupPhase = false
          range = maxSeen - minSeen
          intervalWidth = range/intervals
          individualInterval = ((value-minSeen)/intervalWidth).toInt.min(intervals-1) //TODO: Ugly fix for the fact that the last interval needs to be both sides inclusive
          for(variable <- 0 until variables){
            data(individualInterval)(variable) = -Math.log(Random.nextDouble())
          }
          context.parent ! NodeReady
        }
        else context.parent ! AllReported
      }

  }
  //def allReported(sender: ActorRef): Unit = {
  //  setupReported += sender
  //  if(setupReported.size == 2){
  //    setupReported = Set.empty[ActorRef]
  //    setupIterations += 1
  //    if(setupIterations >= diameter) context.parent ! NodeReady
  //    else context.parent ! AllReported
  //  }
  //}
  def result(): Double = {
    val S: Array[Double] = data.map(_.sum)
    val H: Array[Double] = S.map(a => if (a > 0) (variables-1)/a else 0)
    val S1 = (for(i <- H.indices) yield minSeen + intervalWidth*(i - 1/2)*H(i)).sum
    val S2 = H.sum
    S1/S2
  }
}
