package adventofcode_2018.day1

object Day1Part1 {

  def solve(inputs: List[String]): Int = {
    inputs.foldLeft(0) { (sum, input) ⇒
      sum + input.toInt
    }
  }

}
