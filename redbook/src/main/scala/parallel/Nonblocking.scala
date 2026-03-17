package parallel.nonblock

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.Callable
import parallel.actors.Actor
import parallel.nonblock.Nonblocking.Par._

object Nonblocking {
  type Future[A] = (Either[Exception, A] => Unit) => Unit
  type Par[A] = ExecutorService => Future[A]
  type EitherA[A] = Either[Exception, A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[EitherA[A]]
      val latch = new CountDownLatch(1)

      p(es) { a =>
        ref.set(a); latch.countDown
      }

      latch.await()
      ref.get() match {
        case Right(v) => v
        case Left(e)  => throw e
      }
    }

    def unit[A](a: A): Par[A] =

      es =>
        new Future[A] {
          def apply(cb: EitherA[A] => Unit) = {
            cb(Right(a))
          }
        }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def fork[A](a: => Par[A]): Par[A] =
      es =>
        new Future[A] {
          def apply(cb: EitherA[A] => Unit) =
            eval(es)(a(es)(cb))
        }

    def map[A, B](par: Par[A])(f: A => B) =
      map2(par, unit(f)) { (a, f) => f(a) }

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        new Future[C] {
          def apply(cb: EitherA[C] => Unit): Unit = {
            var ar: Option[A] = None
            var br: Option[B] = None
            // Actor ожидающий оба результата A и B и передающийв cb (callback)
            val combiner = Actor[Either[A, B]](es) {
              case Left(a) =>
                br match {
                  // если br None присваиваем значение ar и ждем br
                  case None => ar = Some(a)
                  // ждать нечего выполняем
                  case Some(b) => eval(es)(cb(eitherThrowable(f(a, b))))
                }
              // Аналогично для ar
              case Right(b) =>
                ar match {
                  case None    => br = Some(b)
                  case Some(a) => eval(es)(cb(eitherThrowable(f(a, b))))
                }
            }
            // Выполяе оба Par через актор, для p оборачиваем в Left, для p2 в Right
            p(es)(a => combiner ! Left(a.right.toOption.get))
            p2(es)(b => combiner ! Right(b.right.toOption.get))
          }
        }
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = { es =>
    if (run(es)(cond)) {
      t(es)
    } else {
      f(es)
    }
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = { es =>
    {
      val index = run(es)(n)
      val p = choices(index)
      p(es)
    }
  }

  def choiceViaN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => {
      val choice: Par[Int] = map(cond) { v =>
        {
          if (v) {
            0
          } else {
            1
          }
        }
      }
      choiceN(choice)(List(t, f))(es)
    }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val p = map2(pa, unit(choices)) { (p, c) => c(p) }
      join(p)(es)
    }

  def chooserChoice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    chooser(cond) { cond =>
      if (cond) {
        t
      } else {
        f
      }
    }
  }

  def chooserChoiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n) { i => choices(i) }

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(a)(es)

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
    join(map2(a, unit(f)) { (a, f) => f(a) })

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

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private def eitherThrowable[A](v: => A) =
    try {
      Right(v)
    } catch {
      case e: Exception => Left(e)
    }
}

object RunApp extends App {
  import Nonblocking._
  import Nonblocking.Par._

  val S = Executors.newFixedThreadPool(8)

  val p1 = unit(1)
  val p2 = unit(2)
  println(run(S)(p1))

  val calcNormal = map2(p1, p2) { (a, b) => a + b }

  val calcEx = map2(p1, p2) { (a, b) =>
    throw new Exception("testddxception")
    a + b
  }

  val simpleBoolean = unit(true)

  val choiced = choice(simpleBoolean)(unit(1), unit(2))

  println(run(S)(calcNormal))
  println(run(S)(choiced))
  println("Choice via n:")
  println(run(S)(choiceViaN(simpleBoolean)(unit(1), unit(2))))
  println("Chooser choice:")
  println(run(S)(chooserChoice(simpleBoolean)(unit(1), unit(2))))
  println("Chooser choice n:")

  println(
    run(S)(
      chooserChoiceN(unit(3))(List(unit(1), unit(2), unit(3), unit(4), unit(5)))
    )
  )

  println("choice n:")
  println(
    run(S)(choiceN(unit(3))(List(unit(1), unit(2), unit(3), unit(4), unit(5))))
  )
}
