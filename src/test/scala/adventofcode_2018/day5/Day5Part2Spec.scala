package adventofcode_2018.day5

import org.scalatest.{ Matchers, WordSpec }
import Day5Part2._

class Day5Part2Spec extends WordSpec with Matchers {

  "solve" must {
    "return right value for example data" in {
      val data = "dabAcCaCBAcCcaDA"
      assert(solve(data) == 4)
    }
  }

}
