package common

import scala.util.Try

object TryTry extends App {

  Try { throw new Exception("Abc") }.get
}
