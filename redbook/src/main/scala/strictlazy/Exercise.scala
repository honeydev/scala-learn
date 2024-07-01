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

  def flatMap[B](f: A => Stream[B]) =
    foldRight[Stream[B]](Empty)((a, acc) => f(a) `merge` acc)
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

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def iter(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, iter(b, a + b))
    iter(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((x, y)) => Stream.cons(x, unfold(y)(f))
      case None         => Empty
    }
  }

  def constant2[A](n: A): Stream[A] =
    Stream.unfold(n)(_ => Some((n, n)))

  def from2(n: Int): Stream[Int] =
    Stream.unfold(n)(s => Some((s, s + 1)))

  def fibs2: Stream[Int] =
    Stream.unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }
}

object Exercise extends App {
  val s = Stream(1, 2, 3)
  val fruits = Seq("apple", "banana", "orange")
  val mapResult = fruits.flatMap(_.toUpperCase)

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
  println(Stream(Stream(1), Stream(2)).flatMap(x => x).toList)
  LazyList(1, 2, 3).map { v =>
    println(s"first f with $v")
    v + 1
  }.map { v =>
    println(s"second f with $v")
    v - 1
  }.toList
  println(Stream.constant(1).take(5).toList)
  println(Stream.from(1).take(10).toList)
  println(Stream.fibs.take(10).toList)
  println(Stream.unfold(0)(s => Some((s, s + 1))).take(4).toList)
  println(Stream.constant2(3).take(6).toList)
  println(Stream.from2(1).take(10).toList)
  println(Stream.fibs2.take(10).toList)
}
