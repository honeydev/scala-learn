package days.two

import cats.syntax.all._
import simulacrum._

@typeclass trait CanTruthy[A] {
  /** Return true, if `a` is truthy. */
  def truthy(a: A): Boolean
}

object CanTruthy {
  def fromTruthy[A](f: A => Boolean): CanTruthy[A] = new CanTruthy[A] {
    def truthy(a: A): Boolean = f(a)
  }
}

object Simulacrum extends App {
  implicit val intCanTruthy: CanTruthy[Int] = CanTruthy.fromTruthy({
    case 0 => false
    case _ => true
  })
  import CanTruthy.ops._
  println(10.truthy)
}
