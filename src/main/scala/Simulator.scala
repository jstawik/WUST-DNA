import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Await
import scala.reflect.ClassTag

import scala.concurrent.duration._
import scala.language.postfixOps

object Simulator {
  implicit val timeout: Timeout = Timeout(5 seconds)
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("simulation")
    val network = system.actorOf(Props[Network], "network")
    maxPropDemo(network)
    system.terminate()
  }

  def maxPropDemo(network: ActorRef): Unit = {
    var trajectory: Seq[Evaluation] = Seq.empty[Evaluation]
    def gridUpdate(): Unit = {
      network ! CommAction("maxPropagation")
      network ! CommAction("plotGrid")
      val future = network ? Evaluate((a: Double, b: Double) => a.max(b))
      trajectory = trajectory appended Await.result(future, timeout.duration).asInstanceOf[Evaluation]
      Thread.sleep(200)
    }
    network ! MakeNetwork[PropagateMax]("grid", Map("side" -> 20))
    for(i <- 1 to 20) gridUpdate()
    Plotter.makeTrajectory(trajectory, "Trajectory demo")
  }
}

