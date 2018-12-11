package adventofcode_2018.day4

object Day4Part2 {

  import Day4Part1._

  def solve(inputs: List[String]): Int = {
    import StatsLogic._

    val guardsStats = calculateStats(inputs)

    val (guardId, minute) = guardsStats.map {
      case (id, stats) ⇒
        val value = if (stats.sleepMinutes.nonEmpty) {
          stats.sleepMinutes.maxBy(_._2)
        } else 0 -> 0 // hack cause any guards don't have sleep time...
        id -> value
    }.maxBy(_._2._2)

    guardId * minute._1
  }

}
