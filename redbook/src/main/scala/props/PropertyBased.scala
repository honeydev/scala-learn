package parallel.propertybased

import state.State
import state.SimpleRNG
import state.RNG
import state.Rand
// import strictlazy.Stream
import parallel.propertybased.Gen.weighted

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(v => f(v).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(listSize => {
      val listOfGens = (1 to listSize).map { _ => this.sample }.toList
      Gen(State.sequence(listOfGens))
    })
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {

    val randomNum = State(Rand.nonNegativeInt).map(i => {
      start + i % (stopExclusive - start)
    })

    Gen(randomNum)
  }

  def unit[A](a: => A): Gen[A] =
    Gen(State(Rand.unit(a)))

  def boolean: Gen[Boolean] =
    Gen(State(Rand.nonNegativeInt).map(_ > 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val sequenceOfG = (1 to n).map { _ =>
      g.sample
    }.toList
    Gen(State.sequence(sequenceOfG))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap { v => if (v) g1 else g2 }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val (gen1, weight1) = g1
    val (gen2, weight2) = g2
    val weightSum = (weight1 + weight2).toInt
    choose(0, weightSum).flatMap { v =>
      if (v <= weight1) gen1 else gen2
    }
  }

}

import Prop._

case class Prop(run: (TestCases, RNG) => Result) {

  def check: Result = ???

  def &&(p: Prop): Prop = new Prop(testCases => {
    val r1 = run(testCases)
    val r2 = p.run(testCases)

    (r1, r2) match {
      case (Falsified(fail1, succ1), Falsified(fail2, succ2)) =>
        Falsified(s"${fail1}\n${fail2}", succ1 + succ2)

      case (Passed, f) => f
      case (f, Passed) => f
      case _           => Passed
    }
  })
}

sealed trait Result {
  def isFalsified: Boolean
}

case class Falsified(failure: FailedCase, success: SuccessCount)
    extends Result {

  override def isFalsified = true
}

case object Passed extends Result {

  override def isFalsified: Boolean = false
}

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = new Prop({ (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0).take(n))
      .map { case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  })

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

object PropertyBased extends App {
  val g = Gen.choose(1, 100)
  println(g.sample.run(SimpleRNG(15L)))
  println(Gen.boolean.sample.run(SimpleRNG(666L)))
  println(Gen.listOfN(4, Gen.choose(1, 100)).sample.run(SimpleRNG(1L)))
  println(
    Gen.choose(1, 100).listOfN(Gen.choose(1, 10)).sample.run(SimpleRNG(1L))
  )
  println(
    Gen
      .weighted((Gen.unit(1), 11), (Gen.unit(2), 20))
      .sample
      .run(SimpleRNG(1213231L))
  )

}
