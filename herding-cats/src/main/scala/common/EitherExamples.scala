package common

import cats.Monad

object EitherExamples extends App {
//  Right(1).orElse { v => throw new Exception("abc")}
  Right(1).flatMap(_ => Right(4))
  Monad
}
