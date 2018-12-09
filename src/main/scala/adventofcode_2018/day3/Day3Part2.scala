package adventofcode_2018.day3

import java.awt.Rectangle

object Day3Part2 {

  type Id = Int
  type Claim = (Id, Rectangle)

  object Claim {
    def fromInput(input: String): Claim = {
      input.split(" ").toList match {
        case idSource :: "@" :: indents :: area :: _ ⇒
          val id = idSource.tail.toInt
          val li :: ti :: _ = indents.dropRight(1).split(",").map(_.toInt).toList
          val w :: h :: _ = area.split("x").map(_.toInt).toList
          (id, new Rectangle(li, ti, w, h))

        case _ ⇒ throw new RuntimeException("Wrong input data")
      }
    }
  }

  def solve(inputs: List[String]): Int = {

    val claims = inputs.map(Claim.fromInput)

    val result = claims.foldLeft(claims) { (accum, claim) ⇒
      val notIntersected = accum.filterNot(c ⇒ (c._1 != claim._1) && (c._2 intersects claim._2))
      if (notIntersected.size == accum.size)
        accum
      else
        notIntersected
    }

    result match {
      case claim :: Nil ⇒ claim._1
      case _            ⇒ throw new RuntimeException("no solution")
    }
  }

}
