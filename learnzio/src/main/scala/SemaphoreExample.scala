import java.util.concurrent.TimeUnit
import zio._
import zio.Console._

val task = for {
  _ <- printLine("start")
  _ <- ZIO.sleep(Duration(2, TimeUnit.SECONDS))
  _ <- printLine("end")
} yield ()

val semTask = (sem: Semaphore) => for {
  _ <- sem.withPermit(task)
} yield ()

val semTaskSeq = (sem: Semaphore) => (1 to 3).map(_ => semTask(sem))

val program = for {

  sem <- Semaphore.make(permits = 1)

  seq <- ZIO.succeed(semTaskSeq(sem))

  _ <- ZIO.collectAllPar(seq)

} yield ()

object SemaphoreExample extends ZIOAppDefault {

  def run = program
}
