package adventofcode_2018.day1

import scala.collection.mutable

object Day1Part2 {

  def solve(inputs: List[String]): Int = {
    val results = mutable.HashSet(0)

    Stream.continually(inputs.map(_.toInt)).foldLeft(0) { (globalSum, values) ⇒
      values.foldLeft(globalSum) { (sum, value) ⇒
        val result = sum + value
        if (results.add(result)) {
          result
        } else {
          return result
        }
      }
    }
  }

}
