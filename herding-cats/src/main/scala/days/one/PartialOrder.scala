package days.one

import cats.syntax.all._

object PartialOrder extends  App {
  println(1.0 tryCompare Double.NaN)
}