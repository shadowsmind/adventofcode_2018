package adventofcode_2018

import adventofcode_2018.day1.{ Day1Part1, Day1Part2 }
import adventofcode_2018.day3.{ Day3Part1, Day3Part2 }
import adventofcode_2018.day4.{ Day4Part1, Day4Part2 }
import adventofcode_2018.day5.{ Day5Part1, Day5Part2 }

import scala.io.{ Source, StdIn }

object Main extends App {

  val NoSolutionMsg = "Solution not implemented"

  def loadInput(path: String): List[String] = {
    val source = Source.fromFile(path)
    val lines = source.getLines().toList
    source.close()

    lines
  }

  println("Enter day number:")
  val day = StdIn.readInt()

  if (day < 1 && day > 25) {
    throw new IllegalArgumentException("Wrong day input, use: 1 - 25")
  }

  println("Enter part number:")
  val part = StdIn.readInt()

  if (part < 1 && day > 2) {
    throw new IllegalArgumentException("Wrong part input, use: 1 - 2")
  }

  println("Enter path to input data:")
  val path = StdIn.readLine()

  if (path.trim.isEmpty) {
    throw new IllegalArgumentException("Path can't be empty")
  }

  val input = loadInput(path)

  val result = day match {
    case 1 ⇒
      part match {
        case 1 ⇒ Day1Part1.solve(input)
        case 2 ⇒ Day1Part2.solve(input)
      }
    case 3 ⇒
      part match {
        case 1 ⇒ Day3Part1.solve(input)
        case 2 ⇒ Day3Part2.solve(input)
      }
    case 4 ⇒
      part match {
        case 1 ⇒ Day4Part1.solve(input)
        case 2 ⇒ Day4Part2.solve(input)
      }
    case 5 ⇒
      part match {
        case 1 ⇒ Day5Part1.solve(input.head)
        case 2 ⇒ Day5Part2.solve(input.head)
      }

    case _ ⇒ NoSolutionMsg
  }

  println(result)

}
