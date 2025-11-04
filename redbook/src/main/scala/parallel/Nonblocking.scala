package parallel.nonblock

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.Callable


object Nonblocking {
  type Future[A] = (A => Unit) => Unit
  type Par[A] = ExecutorService => Future[A]
  
  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown }
      latch.await()
      ref.get
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit) = 
          cb(a) 
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })
  }
}

object RunApp extends App {
  import Nonblocking._

  val S = Executors.newFixedThreadPool(2)

  val u = Par.unit(1)
  val res = Par.run(S)(u)

  println(res)
}

