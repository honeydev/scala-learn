package strictlazy

sealed trait Stream[+A] {

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, tail) => h() :: tail().toList
    }

  def take(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, tail) =>
        if (n > 1)
          Stream.cons(h(), tail().take(n - 1))
        else
          Stream.cons(h(), Empty)
    }

  def takeWhile(p: A => Boolean): Stream[A] =
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

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def headOption: Option[A] = {
    foldRight[Option[A]](None)((a, b) => Some(a))
  }

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[B >: A](b: => B): Stream[B] =
    foldRight(Stream(b))((a, b) => Stream.cons(a, b))

  def merge[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((a, b) => Stream.cons(a, b))
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

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

object Exercise extends App {
  val s = Stream(1, 2, 3)

  println(s.toList)
  println(s.take(2).toList)
  println(s.takeWhile(_ != 2).toList)
  println(s.forAll(_ > 0))
  println(s.takeWhile2(_ != 2).toList)
  println(s.headOption)
  println(s.map(_ + 1).toList)
  println(s.filter(_ != 2).toList)
  println(s.append(4).toList)
  println(s.merge(Stream(4, 5, 6)).toList)
}
