package state

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Rand {

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeInt: Rand[Int] =
    rng => {
      val (randomNum, nextRNG) = rng.nextInt
      (
        if (randomNum < 0) -randomNum else randomNum,
        nextRNG
      )
    }

  // def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
  //   rng => {
  //     val (a, rng2) = s(rng)
  //     (f(a), rng2)
  //   }
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { v => Rand.unit(f(v)) }

  // def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  //   rng => {
  //     val (a, rng1) = ra(rng)
  //     val (b, rng2) = rb(rng1)

  //     (f(a, b), rng2)
  //   }
  //
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { n1 => flatMap(rb) { n2 => unit(f(n1, n2)) } }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    {
      val (innerRand, rng2) = f(rng)

      g(innerRand)(rng2)
    }
  }

  // IMPLEMENT flatMap VIA map
  // def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
  //   rng => {
  //     val (randB, rng2) = map(f)(g)(rng)
  //     randB(rng2)

  //   }
  // }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng =>
    fs.foldLeft((List[A](), rng)) { case ((list, currentRng), rand: Rand[A]) =>
      val (a, rng2) = rand(currentRng)
      (a :: list, rng2)
    }
  }
}

object Exerecise extends App {
  import Rand._

  val int: Rand[Int] = _.nextInt

  def double(rng: RNG) = {
    val (nextVal, nextRNG) = rng.nextInt

    ((nextVal.toDouble / Int.MaxValue).abs, nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (randomInt, nextRNG) = rng.nextInt
    val (randomDouble, nextRNG2) = double(nextRNG)
    ((randomInt, randomDouble), nextRNG2)
  }

  def doubleInt(rng: RNG) =
    intDouble(rng) match {
      case ((i, d), rng) => ((d, i), rng)
    }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count)
      .foldLeft((List[Int](), rng)) {
        case ((acc: List[Int], rng: RNG), _: Int) =>
          val (nextInt, nextRNG) = rng.nextInt
          Tuple2[List[Int], RNG](acc :+ nextInt, nextRNG)
      }
  }

  def double(rand: Rand[Int]): Rand[Double] =
    map(rand)(v => v.toDouble)

  println(double(SimpleRNG(12312312317124L)))
  println(intDouble(SimpleRNG(7123123125L)))
  println(doubleInt(SimpleRNG(7123123125L)))
  println(double3(SimpleRNG(12312312312L)))
  println(ints(10)(SimpleRNG(123123123524L)))
  println(Rand.unit(5))
  println(SimpleRNG(12312345L).nextInt)
  println(double(unit(5)))
  val sequenceRand = sequence(List.fill(5)(Rand.unit(5)))
  println(sequenceRand(SimpleRNG(12345L)))
  println(flatMap(double(unit(5))) { v => unit(v) })
}
