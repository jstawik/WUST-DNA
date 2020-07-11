import akka.actor.{ActorSystem, Props}

import scala.reflect.ClassTag

object Simulator {
  def main(args: Array[String]): Unit = {
    maxPropDemo()
  }

  def maxPropDemo(): Unit = {
    val system = ActorSystem("simulation")
    val network = system.actorOf(Props[Network], "network")
    def gridUpdate(): Unit = {
      network ! CommAction("maxPropagation")
      network ! CommAction("plotGrid")
      Thread.sleep(500)
    }
    network ! MakeNetwork[PropagateMax]("grid", Map("n" -> 500))
    for(i <- 1 to 10) gridUpdate()
    system.terminate()
  }
}

