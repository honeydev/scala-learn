import zio._

object CountRequests extends ZIOAppDefault {

  def request(counter: Ref[Int]): ZIO[Any, Nothing, Unit] = {
    for {
      _ <- counter.update(_ + 1)
      reqNumber <- counter.get
      _ <- Console.printLine(s"request number: $reqNumber").orDie
    } yield ()
  }

  private val initial = 0
  private val myApp =
    for {
      ref <- Ref.make(initial)
      _ <- request(ref) zipPar request(ref)
      rn <- ref.get
      _ <- Console.printLine(s"total requests performed: $rn").orDie
    } yield ()

  def run = myApp
}
