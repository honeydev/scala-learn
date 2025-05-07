package state


trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Exerecise extends App {

  def double(rng: RNG) = {
    val (nextVal, nextRNG) = rng.nextInt

    ((nextVal.toDouble / Int.MaxValue).abs, nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (randomInt, nextRNG) = rng.nextInt
    val (randomDouble, nextRNG2)= double(nextRNG)
    ((randomInt, randomDouble), nextRNG2)
  }

  def doubleInt(rng: RNG) =
    intDouble(rng) match {
      case ((i, d), rng) => ((d, i), rng)
    }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3)  = double(rng2)
    ((double1, double2, double3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      (1 to count)
        .foldLeft((List[Int](), rng)) { case ((acc: List[Int], rng: RNG), _: Int) =>
        val (nextInt, nextRNG) = rng.nextInt
        Tuple2[List[Int], RNG](nextInt :: acc, nextRNG)
      }
  }

  println(double(SimpleRNG(12312312317123L)))
  println(intDouble(SimpleRNG(7123123125L)))
  println(doubleInt(SimpleRNG(7123123125L)))
  println(double3(SimpleRNG(12312312312L)))
  println(ints(10)(SimpleRNG(123123123523L)))
}
