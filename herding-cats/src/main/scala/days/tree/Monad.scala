package days.tree

import cats._
import cats.syntax.all._

object MonadLearn extends App {

    val a: Int = 10
    val res = a.tailRecM[Option, Int](i => if (i == 20) Some(Right(i)) else Some(Left(i+1)))
    println(res)

    val res2 = Some(List(1, 2, 3, 4)).tailRecM[Option, List[Int]] {
      case Some(3 :: 4 :: Nil) => Some(Right(List(1)))
      case Some(h :: tail) => Some(Left(Some(tail)))
    }

    println(res2)
}

