package adventofcode_2018.day4

import java.time.temporal.ChronoUnit
import java.time.{ LocalDateTime, ZoneOffset }

import scala.math.Ordering

object Day4Part1 {

  type GuardId = Int
  type Minute = Int
  type Duty = List[Record]
  type MinutesStats = Map[Minute, Int]
  type GuardsStats = Map[GuardId, SleepStats]

  sealed trait GuardAct
  case class OnDuty(id: GuardId) extends GuardAct
  case object FallsAsleep extends GuardAct
  case object WakesUp extends GuardAct

  case class SleepStats(sleepTotal: Int, sleepMinutes: MinutesStats) {
    def withSleep(from: LocalDateTime, to: LocalDateTime): SleepStats = {
      val (timeSleep, minutesStats) = TimeUtils.calculateSleepTime(from, to)
      SleepStats(this.sleepTotal + timeSleep, this.sleepMinutes |+| minutesStats)
    }
    def update(other: SleepStats): SleepStats =
      SleepStats(this.sleepTotal + other.sleepTotal, this.sleepMinutes |+| other.sleepMinutes)
  }

  object SleepStats {
    def empty: SleepStats = SleepStats(0, Map.empty)
  }

  case class Record(time: LocalDateTime, act: GuardAct) {
    def isStartDuty: Boolean = act match {
      case _: OnDuty ⇒ true
      case _         ⇒ false
    }
  }

  object Record {
    def fromInput(input: String): Record = {
      input.split(" ").toList match {
        case dateInfo :: timeInfo :: actInfo :: maybeId :: _ ⇒
          val act = actInfo match {
            case "Guard" ⇒ OnDuty(maybeId.tail.toInt)
            case "falls" ⇒ FallsAsleep
            case "wakes" ⇒ WakesUp
          }
          val time = LocalDateTime.parse(s"${dateInfo.tail}T${timeInfo.dropRight(1)}")
          Record(time, act)

        case _ ⇒ throw new RuntimeException("wrong input")
      }
    }
  }

  object StatsLogic {
    def calculateSleep(accums: (SleepStats, Option[Record]), record: Record): (SleepStats, Option[Record]) = {
      val (stats, sleepRecord) = accums
      record.act match {
        case FallsAsleep ⇒ (stats, Some(record))
        case _ ⇒
          sleepRecord match {
            case Some(sr) ⇒
              (stats.withSleep(sr.time, record.time), None)
            case None ⇒ (stats, None)
          }
      }
    }

    def groupRecords(accums: (Map[GuardId, Duty], GuardId), record: Record): (Map[GuardId, Duty], GuardId) = {
      val (duties, currentId) = accums
      val guardId = record.act match {
        case OnDuty(i) ⇒ i
        case _         ⇒ currentId
      }
      val duty = duties.get(guardId).fold(List(record))(_ :+ record)
      (duties.updated(guardId, duty), guardId)
    }

    def calculateStats(inputs: List[String]): GuardsStats = {
      val records: Seq[Record] = inputs.map(Record.fromInput).sorted

      val (guardsDuties, _) = records.foldLeft(Map.empty[GuardId, Duty], 0)(groupRecords)

      guardsDuties.map {
        case (guardId, duty) ⇒
          val (stats, _) = duty.foldLeft(SleepStats.empty, Option.empty[Record])(calculateSleep)
          guardId -> stats
      }
    }
  }

  object TimeUtils {
    def minutesPassed(from: LocalDateTime, to: LocalDateTime): Int = {
      ChronoUnit.MINUTES.between(from, to).toInt
    }

    def calculateSleepTime(from: LocalDateTime, to: LocalDateTime): (Int, MinutesStats) = {
      val minutes = minutesPassed(from, to)

      val minutesStats = (0 until minutes)
        .map(from.plusMinutes(_).getMinute)
        .groupBy(m ⇒ m)
        .mapValues(_.size)

      (minutes, minutesStats)
    }
  }

  implicit class MinutesStatsOps(m1: MinutesStats) {
    def |+|(m2: MinutesStats): MinutesStats = {
      if (m1.size <= m2.size) {
        m1.foldLeft(m2) {
          case (my, (k, x)) ⇒
            my.updated(k, my.get(k).fold(x)(_ + x))
        }
      } else {
        m2.foldLeft(m1) {
          case (mx, (k, y)) ⇒
            mx.updated(k, mx.get(k).fold(y)(_ + y))
        }
      }
    }
  }

  implicit val timeOrdering: Ordering[Record] = (x: Record, y: Record) ⇒ x.time.compareTo(y.time)

  def solve(inputs: List[String]): Int = {
    import StatsLogic._

    val guardsStats = calculateStats(inputs)

    val (id, stats) = guardsStats.maxBy(_._2.sleepTotal)
    val mostSleepMinute = stats.sleepMinutes.maxBy(_._2)._1

    id * mostSleepMinute
  }

}
