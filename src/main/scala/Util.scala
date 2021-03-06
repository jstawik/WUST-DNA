import akka.actor.Actor
import akka.util.Timeout
import com.typesafe.scalalogging.Logger

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

trait ActorDefaults extends DefaultLogger with DefaultTimeout

trait DefaultTimeout{implicit val timeout: Timeout = Timeout(1 seconds)}
trait DefaultLogger extends Actor {
  val logger: Logger = Logger(s"${self.path}")
}

object BirdOperator extends Serializable {
  implicit class Pipe[A](a: A) {
    def |>[Z](f: A => Z): Z = f(a)
  }

  implicit class Pipe2[A, B](a: (A, B)) {
    def |>[Z](f: (A, B) => Z): Z = f.tupled(a)
  }
}
