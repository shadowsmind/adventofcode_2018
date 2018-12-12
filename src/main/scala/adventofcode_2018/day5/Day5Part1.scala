package adventofcode_2018.day5

object Day5Part1 {

  def reducePolymer(polymer: String): Int = {
    val chartArray = polymer.toCharArray
    val buffer = chartArray.toBuffer

    chartArray.zipWithIndex.foldLeft(0) {
      case (removedCount, (char, idx)) ⇒
        val currentIdx = idx - removedCount
        val prevIdx = currentIdx - 1
        if (currentIdx != 0) {
          val prev = buffer(prevIdx)
          if (char != prev && char.toUpper == prev.toUpper) {
            buffer.remove(currentIdx)
            buffer.remove(prevIdx)
            removedCount + 2
          } else {
            removedCount
          }
        } else {
          removedCount
        }
    }

    buffer.size
  }

  def solve(data: String): Int = reducePolymer(data)

}
