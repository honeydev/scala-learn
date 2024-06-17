import cats.effect._


object RunEffectsIgnorePrevious extends IOApp.Simple {


  val run =
    IO.println("Hello") >> IO.println("World") >> IO.println("Man")
}
