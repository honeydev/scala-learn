import cats.effect._

import java.util.concurrent.{Executors, TimeUnit}


object SyncAsync extends IOApp.Simple {

  val scheduler = Executors.newScheduledThreadPool(1)

   val run =
     for {
      r <- IO.async_[String] { cb =>
        scheduler.schedule(new Runnable {
          def run = cb(Right("Abc"))
        }, 5000, TimeUnit.MILLISECONDS)
      }
       _ <- IO.println(r).as(ExitCode.Success)
     } yield ()

//  val run = IO {
//    scheduler.schedule(new Runnable {
//      def run = println("abc")
//    }, 5000, TimeUnit.MILLISECONDS)
//    ()
//  } >> IO.println("yz")
}
