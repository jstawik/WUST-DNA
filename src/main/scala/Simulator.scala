import akka.actor.{ActorSystem, Props}

object Simulator {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("simulation")
    val network = system.actorOf(Props[Network], "network")
    def gridUpdate(): Unit = {
      network ! CommAction("maxPropagation")
      network ! CommAction("plotGrid")
      Thread.sleep(3000)
    }
    network ! MakeGrid(50)
    for(i <- 1 to 20) gridUpdate()
    system.terminate()
  }
}
