package days.tree

import cats._
import cats.syntax.all._


object ApplicativeEx extends App {

  // Applicative
  val f: (Int, Char) => Double = (i, c) => (i + c).toDouble

  val int: Option[Int] = Some(5)
  val char: Option[Char] = Some('a')

// int.map(i => (c: Char) => f(i, c)).ap(char)
  val someF: Option[Int => Long] = Some(_.toLong + 1L)
  val someInt: Option[Int] = Some(3)

  someF.ap(someInt)

  val fuu: Option[Int => Int] = Some((x: Int) => 1)
  val F = Applicative[Option]
  val applied = F.ap({ F.pure((_: Int) + 3) })(F.pure(9))
  println(applied)
  println(fuu.ap(Some(4)))
  println(sequenceA(List(1.some, 2.some)))

  val apUsage = Applicative[Option].ap(Some((x: Int) => x + 2))(Some(2))
  val ap2 = Applicative[Option].map2(1.some, 2.some)(_ + _)

  println(apUsage)
  println("Ap2:")
  println(ap2)

  def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = list match {
    case Nil     => Applicative[F].pure(Nil: List[A])
    case x :: xs => (x, sequenceA(xs)) mapN {_ :: _}
  }
}
