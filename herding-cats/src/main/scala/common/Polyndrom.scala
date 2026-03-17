import scala.util.control.Breaks._

object Solution extends App {

  def longestPalindrome(s: String): String = {

    def find(leftIndex: Int, rightIndex: Int): Option[(Int, Int)] = {

      var iterLeftInex = leftIndex
      var iterRightIndex = rightIndex

      while (iterLeftInex < iterRightIndex) {

        if (s(iterLeftInex) != s(iterRightIndex)) {
          return None
        } else {
          iterLeftInex += 1
          iterRightIndex -= 1
        }
      }

      Some((leftIndex, rightIndex))
    }

    var result = ""
    (0 until s.length).reverse.foreach { rightBorderIndex =>
      (0 to rightBorderIndex).foreach { leftBorderIndex =>
        val guess = find(leftBorderIndex, rightBorderIndex)

        val mapped = guess
          .map { case (a, b) => s.slice(a, b + 1) }

        if (mapped.isDefined) {
          val guess = mapped.get
          if (guess.length > result.length) {
            result = guess
          }
        }
      }
    }
    result
  }

}