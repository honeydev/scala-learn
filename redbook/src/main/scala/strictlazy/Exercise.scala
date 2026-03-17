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

  def startsWith[A](s: Stream[A]): Boolean = {
    (this, s) match {
      case (Empty, Cons(_, _)) => false
      case (Cons(a, b), Cons(c, d)) if (a() == c()) => b().startsWith(d())
      case (Cons(_, _), Empty) | (Empty, Empty) => true
      case _                   => false
    }
  }

  def startsWith2[A](s: Stream[A]): Boolean = {
    this.zipAll(s).forAll {
      case (Some(a), Some(b)) => a == b
      case (None, Some(_))    => false
      case (Some(_), None)    => true
    }
  }

  def zipWith[B,C](that: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()) -> None) -> (t1() -> Empty))
      case (Empty, Cons(h2, t2)) => Some((None -> Some(h2())) -> (Empty -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))
    }

  def mapUnfold[B](f: A => B) = {
    Stream.unfold(this) { case Cons(a, b) => Some(f(a()), b()) }
  }

  def takeUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(a, b), n) => if (n > 0) Some((a(), (b(), n - 1))) else None
    }

  def takeWhileUnfold(cond: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(a, b) => if (cond(a())) Some((a(), b())) else None
    }

  def zipWithUnfold[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, that)) {
      case (Cons(a, b), Cons(c, d)) => Some((f(a(), c()), (b(), d())))
      case _                        => None
    }

  def zipAllUnfold[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(a, b), Empty) => Some((Some(a()), None), (b(), Empty))
      case (Empty, Cons(c, d)) => Some((None, Some(c())), (Empty, d()))
      case (Cons(a, b), Cons(c, d)) => Some(((Some(a()), Some(c())), (b(), d())))
    }

  def startsWithUnfold[B](that: Stream[B]): Boolean = {
    ???
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

  def ones: Stream[Int] =
    Stream.unfold(1)(_ => Some((1, 1)))
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
  println(Stream.from2(1).take(3).toList)
  println(Stream.from2(1).take(3).startsWith(Stream(1, 2)))
  println(Stream.from2(1).take(3).startsWith2(Stream(1, 2, 3)))
  // only ones
  println(Stream.ones.take(5).toList)
  // mapUnfold
  println(Stream.from2(1).mapUnfold(_ * 2).take(4).toList)
  println(Stream.from2(0).takeUnfold(4).toList)
  println(Stream.from2(3).takeWhileUnfold(_ < 10).toList)
  println(Stream.from2(10).take(5).zipWithUnfold(Stream.from2(20).take(5)) { _ + _ }.toList)
  print(Stream(1, 2, 3, 4).zipAllUnfold(Stream(2, 3, 4, 5, 6, 8)).toList)
}
