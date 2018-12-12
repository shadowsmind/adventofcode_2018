package adventofcode_2018.day5

object Day5Part2 {

  import Day5Part1._

  def solve(data: String): Int = {
    val polymerSizes = ('a' to 'z').map { char ⇒
      val polymer = data.filterNot(c ⇒ c == char || c == char.toUpper)
      reducePolymer(polymer)
    }

    polymerSizes.min
  }

}
