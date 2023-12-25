package days.eq
import cats.syntax.all._

object Eq extends  App {

  println(1 === 1 && 1.some =!= 2.some)
}
