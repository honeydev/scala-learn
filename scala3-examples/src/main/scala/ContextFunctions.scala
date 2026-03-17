package contextfexample

val increment: Int ?=> Int = summon[Int] + 1

type WrappedAlso[T] = T

def it[T](using a: WrappedAlso[T]) = a

extension [T](x: T)
  def also(f: WrappedAlso[T] ?=> Unit): T =
    f(using x)
    x

  def let(f: WrappedAlso[T] ?=> T): T =
    f(using x)


@main
def main(): Unit = {

  given Int = 1

  increment


  // Kotlin like scope function implementation
  val numbers = List(1, 2, 3, 4, 5)
    numbers
      .also {
        val orginalValue = summon[List[Int]]
        println(s"`function it[T](usign a: WrappedAlso[T])` apply implicitly value from argumnet $it, original value maybe summoned: $orginalValue" ) 
      }
      .also { println(s"The list before adding 6: ${this.it}") }
      .appended(6)
      .also(println(s"The list after adding 6: $it"))
      .let {
        it.filter(_ > 2)
      }
      .also { println(s"List el gt then 2: $it") }
}

