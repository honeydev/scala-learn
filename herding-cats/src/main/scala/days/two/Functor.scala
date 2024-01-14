package days.two

import cats._
import cats.syntax.all._
import simulacrum._

object FunctorEx extends App {
  
  println(Functor[List].map(List(1, 2, 3)) { _ + 1 })
  // void transform functor value to `()`
  println(List(1, 2, 3).void)
  val lifted = Functor[List].lift {(_: Int) * 2}
  // Make function completable to work with functor type
  // In this case make (_: Int) => Init to List[Int] => List[Int]
  println(lifted(List(1, 2, 3)))
  // Apply function to functor value A and return 
  // tuple (A, B) where is A source value and B result of f(a)
  println(List(1, 2, 3).fproduct { (_: Int) * 2 })
  // map functor values to new value
  println(List(1, 2, 3) as "x")
  val x: Either[String, Int] = Right(1)

  assert { (x map identity) === x }

  val f = {(_: Int) * 3}
  // f: Int => Int = <function1>
  
  val g = {(_: Int) + 1}
  // g: Int => Int = <function1>
  
  assert { (x map (f map g)) === (x map f map g) }
}

