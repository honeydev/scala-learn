import cats.{Contravariant, Functor}
import cats.implicits._
import cats.effect._
import cats.effect.std.{Dequeue, DequeueSource, DequeueSink}
import cats.effect.unsafe.implicits.global

object DequeueLearn extends App {
  def covariant(list: List[Int]): IO[List[Long]] = (
    for {
      q <- Dequeue.bounded[IO, Int](10)
      qOfLongs: DequeueSource[IO, Long] = Functor[DequeueSource[IO, *]].map(q)(_.toLong)
      _ <- list.traverse(q.offer(_))
      l <- List.fill(list.length)(()).traverse(_ => qOfLongs.take)
    } yield l
    )

  covariant(List(1,4,2,3)).flatMap(IO.println(_)).unsafeRunSync()

}
