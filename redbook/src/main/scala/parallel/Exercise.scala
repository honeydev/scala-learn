package parallel

import parallel.Par.{UnitFuture, parMap, sequence}

import java.lang.System.currentTimeMillis
import java.util.concurrent.{Callable, ExecutorService, Executors, Future, TimeUnit}
import scala.concurrent

case class Computation[A](value: A)

object Par {
  type Par[A] = ExecutorService => Future[A]

  case class UnitFuture[A](get: A) extends Future[A] {

    def unit[A](a: A): Par[A] = Par.unit(a)
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A) =
    (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = 
    (es: ExecutorService) => {
      val af = a(es)
      val ab = b(es)
      UnitFuture(f(af.get, ab.get))
    }

   def map2Timeouted[A,B,C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
   (es: ExecutorService) => new Future[C] {
     val af = a(es)
     val ab = b(es)
     var cache: Option[C] = None

     override def cancel(mayInterruptIfRunning: Boolean): Boolean = af.cancel(true) || ab.cancel(true)

     override def isCancelled: Boolean = af.isCancelled || ab.isCancelled

     override def isDone: Boolean = cache.isDefined

     override def get(): C = get()

     override def get(timeout: Long, unit: TimeUnit): C = {
       val timeoutMs = TimeUnit.NANOSECONDS.convert(timeout, unit)
       val startTimeMs = currentTimeMillis()
       val avalue = af.get(timeoutMs, TimeUnit.MILLISECONDS)
       val timeAfterA = currentTimeMillis()
       val afterA =  timeoutMs - (timeAfterA - startTimeMs)
       val bvalue = ab.get(afterA, TimeUnit.MILLISECONDS)

       val result = f(avalue, bvalue)
       cache = Some(result)
       result
     }
   }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = es => UnitFuture(a)

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map2(parList, unit(())) { (a, _) => a.sorted }

  def map[A,B](par: Par[A])(f: A => B) =
    map2(par, unit(f)) { (a, f) => f(a) }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps match {
      case Nil => unit(List())
      case _   => map2(ps.head, sequence(ps.tail)) { (a, b) => a +: b }
    }
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
}

object Exercise extends App {
  val executorService = Executors.newSingleThreadExecutor
  val timeouted = Par.map2Timeouted(p,y)((a, b) => a * 3 * b)(executorService).get(3, TimeUnit.MILLISECONDS)
  val listOfPar = List(Par.unit(1), Par.unit(2), Par.unit(3), Par.unit(4))

  println(timeouted)
  println(sequence[Int](listOfPar)(executorService).get)
  println(parMap(List(1, 2, 3, 4)) { a => a + 1 })
}
