import cats.effect._
import scala.concurrent.duration._

object Cancelation extends IOApp.Simple {

  val run = {
    lazy val loop: IO[Unit] = IO.println("Hello, World!") >> loop
    loop.timeout(5.seconds)
  }
}
