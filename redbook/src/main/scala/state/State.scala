package state

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(outerState => {
      val (nextValue, nextState) = run(outerState)
      f(nextValue).run(nextState)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.flatMap(b => State.unit(f(a, b))))

}

object State {

  def unit[S, A](v: A): State[S, A] =
    State(s => (v, s))

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] =
    State(s =>
      ls.foldLeft((List[A](), s)) {
        case ((listAcc, stateChanger), currentState) => {
          val (value, nextStateChanger) = currentState.run(stateChanger)
          (value :: listAcc, nextStateChanger)
        }
      }
    )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {}

object Run extends App {
  val value = State
    .unit[Int => Int, Int](4)
    .flatMap(v => State.unit(v))

  val value2 = State.unit[Int => Int, Int](5)

  val sequenced = State.sequence(List(value2, value2)).run(identity)

  val combined = value.map2(value2)((a, b) => a + b)

  println(value.run(identity))
  println(combined.run(identity))
  println(sequenced)
}
