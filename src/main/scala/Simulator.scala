import akka.actor.{ActorRef, ActorSystem, Props}

import scala.reflect.ClassTag

object Simulator {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("simulation")
    val network = system.actorOf(Props[Network], "network")
    maxPropDemo(network)
    system.terminate()
  }

  def maxPropDemo(network: ActorRef): Unit = {
    def gridUpdate(): Unit = {
      network ! CommAction("maxPropagation")
      network ! CommAction("plotGrid")
      Thread.sleep(1000)
    }
    network ! MakeNetwork[PropagateMax]("grid", Map("side" -> 20))
    for(i <- 1 to 20) gridUpdate()
  }
}

