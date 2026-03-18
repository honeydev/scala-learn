package parallel.propertybased

import state.State
import state.SimpleRNG
import state.RNG
import state.Rand
import parallel.propertybased.Gen.weighted

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(v => f(v).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(listSize => {
      val listOfGens = (1 to listSize).map { _ => this.sample }.toList
      Gen(State.sequence(listOfGens))
    })

  def unsized: SGen[A] = SGen((_) => this)
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
    println(s"Gen List, n: $n")
    val sequenceOfG = (1 to n).map { _ =>
      g.sample
    }.toList

    println(s"Generated seq: $sequenceOfG")
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

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = {

    SGen(forSize(_) map f)
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen { size =>
      forSize(size).flatMap { a => f(a).forSize(size) }
    }
  }

}

object SGen {

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { size =>
    println(s"Size $size")
    Gen.listOfN(size, g)
  }

  def unit[A](a: A): SGen[A] = SGen { _ =>
    Gen.unit(a)
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen[List[A]] { size =>
    Gen.listOfN(size max 1, g)
  }
}

import Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop) = new Prop((max, tc, rng) => {

    val r1 = run(max, tc, rng)
    val r2 = run(max, tc, rng)

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
  type MaxSize = Int

  def run(
      p: Prop,
      maxSize: Int = 100,
      testCases: Int = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis())
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(s => g.forSize(s))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
//      val casesPerSize = (n + (max - 1)) / max
      // casesPerSize.min(max)
      //
      //
      val casesPerSize = 4
      println(s"Max: $max, n: $n, Cases per size: $casesPerSize")
      val props = Stream
        .from(0)
        .take(n.min(max) + 1)
        .map(i => {

          val gen = g(i).sample.run(rng)

          println(s"Generator gen: $gen, i: $i")

          forAll(g(i))(f)
        })

      println(
        s"All propes ${props.toList.map(g => g.run(max, casesPerSize, rng))}"
      )

      val propAll = props
        .map(p =>
          Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }
        )
        .toList

      println(s"Prop all: ${propAll.map(g => g.run(max, casesPerSize, rng))}")

      val combined = propAll
        .reduce(_ && _)

      println(s"Combined: ${combined.run(max, casesPerSize, rng)}")

      combined.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = new Prop({ (max, n, rng) =>
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
    Stream.unfold(rng)(rng => {
      val result = Some(g.sample.run(rng))
      println(s"Gen result $result")
      result
    })

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

object PropertyBased extends App {
//  val g = Gen.choose(1, 100)
//  println(g.sample.run(SimpleRNG(15L)))
//  println(Gen.boolean.sample.run(SimpleRNG(666L)))
//  println(Gen.listOfN(4, Gen.choose(1, 100)).sample.run(SimpleRNG(1L)))
//  println(
//    Gen.choose(1, 100).listOfN(Gen.choose(1, 10)).sample.run(SimpleRNG(1L))
//  )
//  println(
//    Gen
//      .weighted((Gen.unit(1), 11), (Gen.unit(2), 20))
//      .sample
//      .run(SimpleRNG(1213231L))
//  )
//
  val sortedGen = SGen.listOf(Gen.choose(2, 10))

  val sortedListProperty = Prop.forAll(sortedGen) { v =>
    val sl = v.sorted
    println(s"Generated value in validator: $v")
    true
  }

  sortedListProperty.run(3, 3, SimpleRNG(15L))

  println(sortedGen.forSize(4).sample.run(SimpleRNG(15L)))

//  run(sortedListProperty, 3, 3)

}
