package adventofcode_2018.day1

import org.scalatest.{ Matchers, WordSpec }
import Day1Part2._

class Day2Part2Spec extends WordSpec with Matchers {

  "solve" must {
    "return right value for example data" in {
      val examples = Seq(("+1, -2, +3, +1", 2), ("+1, -1", 0), ("+3, +3, +4, -2, -4", 10), ("-6, +3, +8, +5, -6", 5), ("+7, +7, -2, -7, -4", 14))
      examples.foreach {
        case (data, result) ⇒
          val inputs = data.split(", ").toList
          assert(solve(inputs) == result)
      }
    }
  }

}
