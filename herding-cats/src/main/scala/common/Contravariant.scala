package common

import cats._
import cats.syntax.all._

trait Z[+T] {

}

object Contravariant extends App {

  case class Money(amount: Int)
  case class Salary(size: Money)

  implicit val showMoney: Show[Money] = Show.show(m => s"$$${m.amount}")
  // implicit val showSalary: Show[Salary] = showMoney.contramap { salary => salary.size }
  // implicit val showSalary: Show[Salary] = { v => "x" }
//  implicit val zx: Z[Salary] = { v: Int => "x"}


//  println(Salary(Money(1000)).show)
}
