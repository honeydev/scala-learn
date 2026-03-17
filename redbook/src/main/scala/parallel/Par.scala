package parallel.par

import java.lang.System.currentTimeMillis
import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  case class UnitFuture[A](get: A) extends Future[A] {
    def unit[A](a: A): Par[A] = {
      Par.unit(a)
    }
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = {
      get
    }
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A) =
    (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val ab = b(es)
      UnitFuture(f(af.get, ab.get))
    }

  def map2Timeouted[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) =>
      new Future[C] {
        val af = a(es)
        val ab = b(es)
        var cache: Option[C] = None

        override def cancel(mayInterruptIfRunning: Boolean): Boolean =
          af.cancel(true) || ab.cancel(true)

        override def isCancelled: Boolean = af.isCancelled || ab.isCancelled

        override def isDone: Boolean = cache.isDefined

        override def get(): C = get()

        override def get(timeout: Long, unit: TimeUnit): C = {
          val timeoutMs = TimeUnit.NANOSECONDS.convert(timeout, unit)
          val startTimeMs = currentTimeMillis()
          val avalue = af.get(timeoutMs, TimeUnit.MILLISECONDS)
          val timeAfterA = currentTimeMillis()
          val afterA = timeoutMs - (timeAfterA - startTimeMs)
          val bvalue = ab.get(afterA, TimeUnit.MILLISECONDS)

          val result = f(avalue, bvalue)
          cache = Some(result)
          result
        }
      }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = {
          println(s"Im get $es")
          a(es).get
        }
      })

  def lazyUnit[A](a: => A): Par[A] = es => UnitFuture(a)

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(())) { (a, _) => a.sorted }

  def map[A, B](par: Par[A])(f: A => B) =
    map2(par, unit(f)) { (a, f) => f(a) }

  def sequence[A](ps: Seq[Par[A]]): Par[List[A]] = {
    ps match {
      case Nil => unit(List())
      case _   => map2(ps.head, sequence(ps.tail)) { (a, b) => a +: b }
    }
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    Par
      .map(parMap(as)((v) => (v, f(v))))((l) =>
        l.filter { case (a, b) => b }.map { case (a, b) => a }
      )
  }

  // Clear solution from answers
  // def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork:
  // val pars: List[Par[List[A]]] =
  //   l.map(asyncF(a => if f(a) then List(a) else List()))
  // sequence(pars).map(_.flatten) // convenience method on `List` for concatenating a list of lists
  //
  //
  def parFold[A, B, C](
      l: Seq[A],
      init: C
  )(s: A => B)(f: (C, B) => C): Par[C] = {
    val pars = l.map(asyncF(s))

    Par.map(sequence(pars))(_.foldLeft(init)(f))
  }

  def maxString(l: Seq[String]): Par[String] =
    Par.parFold(l, "")((v) => (v, v.size)) { case (acc, (v, s)) =>
      if (s > acc.size) v else acc
    }

  def maxInt(l: Seq[Int]): Par[Int] =
    Par.parFold(l, 0)(identity) { case (acc, v) => if (v > acc) v else acc }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = {
    val p1r = p(e).get
    val p2r = p2(e).get
    p1r == p2r
  }
}

object Exercise extends App {
  val listOfPar = List(Par.unit(1), Par.unit(2), Par.unit(3), Par.unit(4))

  println(Par.sequence[Int](listOfPar)(S).get)
  val S = Executors.newFixedThreadPool(2)
  println(Par.parMap(List(1, 2, 3, 4)) { a => a + 1 }(S).get)
  println(Par.parFilter(List(1, 2, 3, 4)) { a => a % 2 == 0 }(S).get)
  println(Par.maxString(List("a", "ab", "c", "easdas"))(S).get)

  val a = Par.lazyUnit(42 + 1)
  println(Par.equal(S)(Par.fork(a), Par.fork(a)))

  // es => es.submit(new Callable[A] {
  //  es =>  es.submit(new Callable[A] {
  //     es => es.submit(new Callable[A] {
  //         def call = {
  //           println(s"Im get $es")
  //           a(es).get
  //         }
  //       }
  //     )
  //  }
  // }
//  val deadlock = Par.fork(Par.fork(Par.fork(a)))(S)
//
//  deadlock.get()

  println("Never End")
}
