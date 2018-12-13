package adventofcode_2018.day1

import org.scalatest.{ Matchers, WordSpec }
import Day1Part1._

class Day1Part1Spec extends WordSpec with Matchers {

  "solve" must {
    "return right value for example data" in {
      val inputs = "+1, -2, +3, +1".split(", ").toList
      assert(solve(inputs) == 3)
    }
  }

}
