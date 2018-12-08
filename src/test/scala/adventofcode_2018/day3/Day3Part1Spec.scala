package adventofcode_2018.day3

import Day3Part1._
import org.scalatest.{ Matchers, WordSpec }

class Day3Part1Spec extends WordSpec with Matchers {

  "Claim" when {
    "fromInput" must {
      "return right claim for right data" in {
        val data = "#1 @ 1,3: 4x4"
        val parsedClaim = Claim.fromInput(data)
        val rightClaim = Claim(1, 3, 4, 4)

        assert(rightClaim == parsedClaim)
      }
    }
  }

  "Section" when {
    "size" must {
      "return right size" in {
        assert(Occupied(1, 3).size == 3)
        assert(Occupied(4, 4).size == 1)
        assert(Free(5, 10).size == 6)
        assert(Intersection(11, 15).size == 5)
      }
    }

    "notIncluded" must {
      "return true when section not includes part or full other section" in {
        val section = Free(1, 10)
        val occupied = Occupied(11, 15)

        assert(section.notIncluded(occupied))
      }
    }

    "combine" must {

      /*
      |-------| (any)
                         combine => ( |###| , [ |-------| ] )
                 |###|
       */
      "any section not included other section" in {
        val section = Free(1, 10)
        val occupied = Occupied(11, 15)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.contains(occupied))
        assert(line == List(section))
      }

      /*
      |*******|
                 combine => ( None , [ |#######| ] )
      |#######|
       */
      "free section with fully matched occupied section" in {
        val section = Free(2, 15)
        val occupied = Occupied(2, 15)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.isEmpty)
        assert(line == List(occupied))
      }

      /*
      |*******|
                 combine => ( None , [ |####|, |***| ] )
      |####|
       */
      "free section with occupied with same start and included end" in {
        val section = Free(1, 7)
        val occupied = Occupied(1, 4)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.isEmpty)
        assert(line == List(occupied, Free(5, 7)))
      }

      /*
      |********|
                 combine => ( None , [ |**|, |###|, |***| ] )
        |###|
       */
      "free section divided by occupied section" in {
        val section = Free(1, 8)
        val occupied = Occupied(3, 5)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.isEmpty)
        assert(line == List(Free(1, 2), occupied, Free(6, 8)))
      }

      /*
      |*****|
                  combine => ( |###| , [ |**|, |###| ] )
        |######|
       */
      "free section with occupied section with longer length" in {
        val section = Free(1, 5)
        val occupied = Occupied(3, 8)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.contains(Occupied(6, 8)))
        assert(line == List(Free(1, 2), Occupied(3, 5)))
      }

      /*
      |#######|
                 combine => ( None , [ |XXXXXXX| ] )
      |#######|
       */
      "occupied section with fully matched with other occupied" in {
        val section = Occupied(5, 8)
        val occupied = Occupied(5, 8)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.isEmpty)
        assert(line == List(Intersection(5, 8)))
      }

      /*
      |#######|
                 combine => ( None , [ |XXXX|, |###| ] )
      |####|
       */
      "occupied section with occupied with same start and included end" in {
        val section = Occupied(1, 7)
        val occupied = Occupied(1, 4)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.isEmpty)
        assert(line == List(Intersection(1, 4), Occupied(5, 7)))
      }

      /*
      |########|
                 combine => ( None , [ |XXX|, |###|, |XX| ] )
         |###|
       */
      "occupied section divided by occupied section" in {
        val section = Occupied(1, 8)
        val occupied = Occupied(4, 6)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.isEmpty)
        assert(line == List(Occupied(1, 3), Intersection(4, 6), Occupied(7, 8)))
      }

      /*
      |#####|
                  combine => ( |###| , [ |##|, |XXX| ] )
        |######|
       */
      "occupied section with occupied section with longer length" in {
        val section = Occupied(1, 5)
        val occupied = Occupied(3, 8)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.contains(Occupied(6, 8)))
        assert(line == List(Occupied(1, 2), Intersection(3, 5)))
      }

      /*
      |XXXXXXX|
                 combine => ( None , [ |XXXXXXX| ] )
      |#######|
       */
      "intersection with fully matched with occupied" in {
        val section = Intersection(13, 22)
        val occupied = Occupied(13, 22)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.isEmpty)
        assert(line == List(Intersection(13, 22)))
      }

      /*
      |XXXXXXX|
                 combine => ( None , [ |XXXXXXX| ] )
      |####|
       */
      "intersection section with occupied with same start and included end" in {
        val section = Intersection(1, 7)
        val occupied = Occupied(1, 4)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.isEmpty)
        assert(line == List(Intersection(1, 7)))
      }

      /*
      |XXXXXXXX|
                 combine => ( None , [ |XXXXXXXX| ] )
         |###|
       */
      "intersection section divided by occupied section" in {
        val section = Intersection(1, 8)
        val occupied = Occupied(4, 6)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.isEmpty)
        assert(line == List(Intersection(1, 8)))
      }

      /*
      |XXXXX|
                  combine => ( |###| , [ |XXXXX| ] )
        |######|
       */
      "intersection section with occupied section with longer length" in {
        val section = Intersection(1, 5)
        val occupied = Occupied(3, 8)
        val (surplus, line) = section.combine(occupied)

        assert(surplus.contains(Occupied(6, 8)))
        assert(line == List(Intersection(1, 5)))
      }
    }
  }

  "Holst" when {
    "intersectionSquare" must {
      "return right result for holst" in {
        val holst = Map(
          1 -> List(Free(1, 8), Intersection(9, 15), Occupied(16, 20)),
          2 -> List(Free(1, 8), Intersection(9, 15), Occupied(16, 20)),
          3 -> List(Intersection(1, 8), Intersection(9, 15), Free(16, 20)),
          4 -> List(Intersection(1, 8), Intersection(9, 15), Free(16, 20))
        )

        assert(holst.intersectionSquare == 44)
      }
    }
  }

  "Day3Part1.solve" must {
    "return right intersection square for example input" in {
      val input =
        """
          |#1 @ 1,3: 4x4
          |#2 @ 3,1: 4x4
          |#3 @ 5,5: 2x2
        """.stripMargin.split("\n").toList.filter(_.trim.nonEmpty)

      val result = solve(input)

      assert(result == 4)
    }
  }

}
