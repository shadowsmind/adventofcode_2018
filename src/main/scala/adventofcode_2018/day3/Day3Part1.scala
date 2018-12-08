package adventofcode_2018.day3

import scala.collection.mutable

object Day3Part1 {

  import Line.LineOps

  type Line = List[Section]
  type Holst = collection.Map[Int, Line]

  implicit class HolstOps(holst: Holst) {
    def intersectionSquare: Int = {
      holst.foldLeft(0) {
        case (square, (_, line)) ⇒
          val intersectionSize = line.foldLeft(0) { (count, section) ⇒
            section match {
              case i: Intersection ⇒ count + i.size
              case _               ⇒ count
            }
          }

          square + intersectionSize
      }
    }
  }

  object Line {
    def empty: Line = List.empty[Section]
    def apply(sections: Section*): Line = sections.toList
    def fromFirstClaim(claim: Claim): Line = {
      if (claim.startColumnNumber != 1) {
        Line(Free(1, claim.startColumnNumber - 1), claim.occupied)
      } else {
        Line(claim.occupied)
      }
    }

    implicit class LineOps(line: Line) {
      def mergeClaim(claim: Claim): Line = {
        val lastSection = line.last
        if (lastSection.notIncluded(claim.occupied)) { // just place to end of line
          val freeSection = Free(lastSection.end + 1, claim.startColumnNumber - 1)
          line :+ freeSection :+ claim.occupied
        } else if (lastSection.included(claim.occupied)) { // merge with last section
          val (surplus, additionalLine) = lastSection |+| claim.occupied
          val surplusLine = surplus.fold[Line](Nil)(s ⇒ List(s))
          line.dropRight(1) ++ additionalLine ++ surplusLine
        } else { // brut force for merge
          val (surplus, mergedLine) = line.foldLeft((Option(claim.occupied), Line.empty)) {
            case ((surplusAccum, lineAccum), section) ⇒
              surplusAccum match {
                case Some(occupied) ⇒
                  val (s, l) = section |+| occupied
                  (s, lineAccum ++ l)

                case None ⇒ (None, lineAccum :+ section)
              }
          }

          val surplusLine = surplus.fold[Line](Nil)(s ⇒ List(s))
          mergedLine ++ surplusLine
        }
      }
    }
  }

  sealed trait Section {
    val start, end: Int

    def size: Int = end - start + 1

    def isEmpty: Boolean = !isDefined
    def isDefined: Boolean = size > 0

    def included(other: Section): Boolean = other.start >= start && other.start <= end
    def notIncluded(other: Section): Boolean = other.start > end
    def fullyMatched(other: Section): Boolean = other.start == start && other.end == end

    /*

    * - free point, # - occupied point, X - intersection point (occupied by many)

    ***###XXX###*** +
    **#####******** =
    **#XXXXXX******

      [ Free(1-3), Occupied(4-6), Intersection(7-9), Free(10-12) ]
    + Occupied(3-7)
    = [ Free(1-2), Occupied(3-3), Intersection(4-9), Free(10-12) ]

    Occupied(3-7)  + Free(1-3)         = Occupied(4-7), [ Free(1-2), Occupied(3-3) ]
    Occupied(4-7)  + Occupied(4-6)     = Occupied(7-7), [ Intersection(4-6) ]
    Occupied(7-7)  + Intersection(7-9) = None,          [ Intersection(7-9) ]
    None           + Free(10-12)       = None,          [ Free(10-12) ]

    */
    def combine(occupied: Occupied): (Option[Occupied], Line) = {
      if (notIncluded(occupied)) {
        (Some(occupied), List(this))
      } else {
        this match {
          case f: Free ⇒
            if (fullyMatched(occupied)) {
              // *******
              // #######
              (None, List(occupied))
            } else if (f.start == occupied.start && f.end > occupied.end) {
              // *******
              // ###
              (None, List(occupied, f.copy(start = occupied.end + 1)))
            } else if (f.start == occupied.start && f.end < occupied.end) {
              // ****
              // #########
              (Some(occupied.copy(start = f.end + 1)), List(occupied.copy(end = f.end)))
            } else if (f.start < occupied.start && f.end < occupied.end) {
              // ******
              //    #######
              (Some(occupied.copy(start = f.end + 1)), List(f.copy(end = occupied.start - 1), occupied.copy(end = f.end)))
            } else {
              // *********
              //    ###
              (None, List(f.copy(end = occupied.start - 1), occupied, f.copy(start = occupied.end + 1)))
            }

          case o: Occupied ⇒
            if (fullyMatched(occupied)) {
              // #######
              // #######
              (None, List(o.toIntersection))
            } else if (o.start == occupied.start && o.end > occupied.end) {
              // #######
              // ###
              (None, List(occupied.toIntersection, o.copy(start = occupied.end + 1)))
            } else if (o.start == occupied.start && o.end < occupied.end) {
              // ####
              // #########
              (Some(occupied.copy(start = o.end + 1)), List(o.toIntersection))
            } else if (o.start < occupied.start && o.end < occupied.end) {
              // ######
              //    #######
              (Some(occupied.copy(start = o.end + 1)), List(o.copy(end = occupied.start - 1), occupied.copy(end = o.end).toIntersection))
            } else {
              // ##########
              //    ###
              (None, List(o.copy(end = occupied.start - 1), occupied.toIntersection, o.copy(start = occupied.end + 1)))
            }
          case i: Intersection ⇒
            if (fullyMatched(occupied)) {
              // XXXXXXX
              // #######
              (None, List(i))
            } else if (i.start == occupied.start && i.end > occupied.end) {
              // XXXXXXX
              // ###
              (None, List(i))
            } else if (i.start == occupied.start && i.end < occupied.end) {
              // XXXX
              // #########
              (Some(occupied.copy(start = i.end + 1)), List(i))
            } else if (i.start < occupied.start && i.end < occupied.end) {
              // XXXXXXX
              //    #######
              (Some(occupied.copy(start = i.end + 1)), List(i))
            } else {
              // XXXXXXXXXX
              //    ###
              (None, List(i))
            }
        }
      }
    }

    def |+|(occupied: Occupied): (Option[Occupied], Line) = combine(occupied)
  }

  case class Intersection(start: Int, end: Int) extends Section
  case class Free(start: Int, end: Int) extends Section
  case class Occupied(start: Int, end: Int) extends Section {
    def toIntersection: Intersection = Intersection(start, end)
  }

  case class Claim(leftIndent: Int, topIndent: Int, weight: Int, height: Int) {
    val startLineNumber = topIndent + 1
    val endLineNumber = topIndent + height
    val startColumnNumber = leftIndent + 1
    val endColumnNumber = leftIndent + weight

    val occupied: Occupied = {
      val startColumnNumber = leftIndent + 1
      val endColumnNumber = leftIndent + weight

      Occupied(startColumnNumber, endColumnNumber)
    }
  }

  object Claim {
    // input example: #1 @ 1,3: 4x4
    def fromInput(input: String): Claim = {
      input.split(" ").toList match {
        case _ :: "@" :: indents :: area :: _ ⇒
          val li :: ti :: _ = indents.dropRight(1).split(",").map(_.toInt).toList
          val w :: h :: _ = area.split("x").map(_.toInt).toList
          Claim(li, ti, w, h)

        case _ ⇒ throw new RuntimeException("Wrong input data")
      }
    }
  }

  def solve(input: List[String]): Int = {
    val holst = mutable.HashMap.empty[Int, Line] // [ line number -> line ]

    input.foreach { data ⇒
      val claim = Claim.fromInput(data)

      for (lineNumber ← claim.startLineNumber to claim.endLineNumber) {
        holst.get(lineNumber) match {
          case Some(currentLine) ⇒
            val line = currentLine.mergeClaim(claim)
            holst.update(lineNumber, line)

          case None ⇒
            val line = Line.fromFirstClaim(claim)
            holst.update(lineNumber, line)
        }
      }
    }

    holst.intersectionSquare
  }

}
