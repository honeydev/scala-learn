package strictlazy


sealed trait Stream[+A] {

  def toList: List[A] =
    this match {
      case Empty         => Nil
      case Cons(h, tail) => h() :: tail().toList
    }

  def take(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, tail) => {
        if (n > 1) {
          Stream.cons(h(), tail().take(n - 1))
        } else {
          Stream.cons(h(), Empty)
        } 
      }
    }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, tail) => {
        val hValue = h() 
        if (p(hValue))
          Stream.cons(hValue, tail().takeWhile(p))
        else
          tail().takeWhile(p)
      }
    }
  }
}
    

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}

object Exercise extends App {
  val s = Stream(1, 2, 3)

  println(s.toList)
  println(s.take(2).toList)
  println(s.takeWhile(_ != 2).toList)
}

