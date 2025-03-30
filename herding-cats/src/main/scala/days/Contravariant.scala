package days

import cats._
import cats.syntax.all._

object Contravariant extends App {

  case class Money(amount: Int, test: Long)
  case class Salary(size: Money)


  implicit val showMoney: Show[Money] = Show.show(m => s"$$${m.amount}")
  implicit val showSalary: Show[Salary] = showMoney.contramap(_.size)
  // showSalary: Show[Salary] = cats.Show$$anon$2$$Lambda$12733/0x00007fc66e2e4478@79a53a7a

  val s = Salary(Money(1000, 14))
  println(s.show)
}
