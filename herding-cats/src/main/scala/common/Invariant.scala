package common

object Invariant extends App {

  import java.util.Date

  // import everything for simplicity:
  import cats._
  import cats.syntax.all._

  def longToDate: Long => Date = new Date(_)
  def dateToLong: Date => Long = { x =>
    x.getTime
  }

  implicit val semigroupDate: Semigroup[Date] =
    Semigroup[Long].imap(longToDate)(dateToLong)

  val today: Date = longToDate(1449088684104l)
  val timeLeft: Date = longToDate(1900918893l)
  println(today |+| timeLeft)
}
