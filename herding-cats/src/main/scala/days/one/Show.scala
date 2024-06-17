package days.one

import cats.Show
import cats.syntax.all._

object ShowExample extends App {
  println(3.show)
  println("Hello".show)
  // Show implementation
//  object Show {
//    /** creates an instance of [[Show]] using the provided function */
//    def show[A](f: A => String): Show[A] = new Show[A] {
//      def show(a: A): String = f(a)
//    }
//
//    /** creates an instance of [[Show]] using object toString */
//    def fromToString[A]: Show[A] = new Show[A] {
//      def show(a: A): String = a.toString
//    }
//
//    implicit val catsContravariantForShow: Contravariant[Show] = new Contravariant[Show] {
//      def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
//        show[B](fa.show _ compose f)
//    }
//  }

  {
    // Use user defined function
    implicit val personShow = Show.show[Person](_.name)
    println(("alice").show)
    // Use toString function
    implicit val carShow = Show.fromToString[Car]
    println(Car("Toyota").show)
    println(Car("Toyota").show === Car("Toyota").toString)
  }
}

case class Person(name: String)
case class Car(model: String)

