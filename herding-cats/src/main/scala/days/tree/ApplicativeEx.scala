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
  val noneF: Option[Int => Long] = None
  val someInt: Option[Int] = Some(3)
  val noneInt: Option[Int] = None
   
  someF.ap(someInt)



  val fuu: Option[Int => Int] = Some((x: Int) => 1)


  println(fuu.ap(Some(4)))
}

