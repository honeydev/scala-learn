package days.one

import cats.kernel.Eq
import cats.syntax.all._

trait MyEq[T]

object Typeclasses extends App {
  sealed trait TrafficLight
  object TrafficLight {
    def red: TrafficLight = Red
    def yellow: TrafficLight = Yellow
    def green: TrafficLight = Green
    case object Red extends TrafficLight
    case object Yellow extends TrafficLight
    case object Green extends TrafficLight
  }

  {
    implicit val trafficLightEq: Eq[TrafficLight] =
      new Eq[TrafficLight] {
        def eqv(a1: TrafficLight, a2: TrafficLight): Boolean = a1 == a2
      }
      println(TrafficLight.red === TrafficLight.yellow)
  }
  {
    implicit val eq: Eq[TrafficLight] = Eq.fromUniversalEquals[TrafficLight]
    println(TrafficLight.red === TrafficLight.red)
  }
}
