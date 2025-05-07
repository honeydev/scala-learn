package common

import scala.sys.error
import scala.util.{Failure, Try}

object TryExamples extends App {
  val value = Try { new Exception("Abc") }
    .recoverWith { e => println(e)
      Failure(e)
    }

  Option.empty

  println(None.toRight(Left("error")))

  println(value)
}
