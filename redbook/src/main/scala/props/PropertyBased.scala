package parallel.propertybased

import org.scalacheck._

object PropertyBased extends App {

  val intList = Gen.listOf(Gen.choose(0, 100))

  val prop = Prop.forAll(intList) { l =>
    val sumEqual = l.reverse.sum == l.sum

    val ifSameElement = if (l.nonEmpty && l.forall(_ == l.head)) {
      l.sum == math.pow(l.head, l.size)
    } else {
      true
    }

    sumEqual && ifSameElement
  }

  prop.check
}
