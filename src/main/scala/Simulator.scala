import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}

object Simulator{
  val system: ActorSystem = ActorSystem("simulation")
  def main(args: Array[String]): Unit = {
    //system.actorOf(Props[MaxPropDemo], "demo")
    system.actorOf(Props[ACDemo], "demo")
  }
}

//class MaxPropDemo extends Actor with ActorDefaults {
//  val network: ActorRef = context.actorOf(Props[Network], "network")
//  val iterations: Int = 5
//  val side: Int = 3
//  network ! MakeNetwork[PropagateMax]("grid", Map("side" -> side))
//
//  var currentIteration = 0
//  var trajectory: Seq[Evaluation[Double]] = Seq.empty[Evaluation[Double]]
//
//  def receive: Receive = {
//    case Evaluation(f, a, mx, mn, avg) =>
//      trajectory = trajectory appended Evaluation[Double](f, a.asInstanceOf[Acc[Double]], mx, mn, avg)
//    case NetworkReady =>
//      logger.debug(s"NetworkReady received by ${self.path}, running first step")
//      network ! SingleStep
//    case AllReported =>
//      if (currentIteration < iterations) {
//        network ! PlotGrid(currentIteration)
//        network ! Evaluate(currentIteration, Acc[Double](0), (a: Acc[Double], b: Double) => Acc[Double](a.acc.max(b)))
//        currentIteration += 1
//        network ! SingleStep
//      } else {
//        Plotter.makeTrajectory[Double](trajectory, "Trajectory demo", (a: Acc[Double]) => a.acc)
//        context.parent ! PoisonPill
//      }
//    case m @_ => logger.error(s"Unhandled message from ${sender().path}: $m")
//  }
//}

class ACDemo extends Actor with ActorDefaults{
  val network: ActorRef = context.actorOf(Props[Network], "network")
  val t0: Long = System.nanoTime
  val iterations: Int = 15

  val side_a: Int = 40
  val side_b: Int = 25
  val count: Int = side_a * side_b
  network ! MakeNetwork[AverageCounting]("grid", Map("side_a" -> side_a, "side_b" -> side_b))
  //val count = 10
  //val n = 3
  //network ! MakeNetwork[AverageCounting]("NRegular", Map("count" -> count, "n" -> n))

  var setupPhase: Boolean = true
  var currentIteration = 0
  var trajectory: Seq[Evaluation] = Seq.empty
  val eval_function: (Acc[(Double, Int)], Double) => Acc[(Double, Int)] = (a, b) => Acc(a.acc._1 + b, a.acc._2 + 1)
  def receive: Receive = {
    case NetworkReady =>
      logger.debug(s"NetworkReady received by ${self.path}")
      network ! SingleStep
    case AllReady =>
      setupPhase = false
      network ! SingleStep
    case AllReported =>
      if(setupPhase) network ! SingleStep
      else {
        if (currentIteration < iterations) {
          //network ! PlotGrid(currentIteration)
          network ! Evaluate(currentIteration)
          currentIteration += 1
          network ! SingleStep
        }
        else {
          Plotter.makeTrajectory(trajectory, "Trajectory demo", (a: Double) => a / count)
          println(s"Total time for $count nodes: ${(System.nanoTime - t0)/1e9d}")
          context.parent ! PoisonPill
        }
      }
    case Evaluation(f, as, rs) => trajectory = trajectory appended Evaluation(f, as, rs)
    case m @_ => logger.error(s"Receive: Unhandled message from ${sender().path}: $m")
  }

}
