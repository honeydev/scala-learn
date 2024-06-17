import cats.effect._
import cats.implicits.catsSyntaxTuple5Parallel

object Concurrency extends IOApp.Simple {

  val run =
    for {
      _ <- IO.println("Always before")
      _ <- (IO.println("A"), IO.println("B"), IO.println("C"), IO.println("D"), IO.println("E")).parTupled
      _ <- IO.println("Always after")
    } yield ()
}
