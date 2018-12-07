import java.nio.file.{Files, Paths}
import java.time.{Duration, LocalDateTime}

import scala.collection.JavaConverters._

object Main extends App {
  abstract class EventType
  object BeginShift extends EventType
  object FallsAsleep extends EventType
  object WakesUp extends EventType
  case class Event(when: LocalDateTime, id: Option[Int], t: EventType)
  {
    def withId(i: Int): Event = Event(when, Some(i), t)
  }

  object Event
  {
    val pattern = "\\[([^\\]]+)\\]\\s(.*)$".r
    val beginShiftPattern = "Guard #(\\d+) begins shift".r
    def parse(s: String): Event =
      s match {
        case pattern(timeStr, eventStr) =>
          val time = LocalDateTime.parse(timeStr.replace(' ', 'T'))
          val (event, id) = eventStr match {
            case "falls asleep" => (FallsAsleep, None)
            case "wakes up" => (WakesUp, None)
            case _ => eventStr match {
              case beginShiftPattern(idStr) => (BeginShift, Some(Integer.parseInt(idStr)))
            }
          }
          Event(time, id, event)
      }
  }




  val lines = Files.readAllLines(Paths.get("input")).asScala
  val testLines = List(
"[1518-11-01 00:00] Guard #10 begins shift",
"[1518-11-01 00:05] falls asleep",
"[1518-11-01 00:25] wakes up",
"[1518-11-01 00:30] falls asleep",
"[1518-11-01 00:55] wakes up",
"[1518-11-01 23:58] Guard #99 begins shift",
"[1518-11-02 00:40] falls asleep",
"[1518-11-02 00:50] wakes up",
"[1518-11-03 00:05] Guard #10 begins shift",
"[1518-11-03 00:24] falls asleep",
"[1518-11-03 00:29] wakes up",
"[1518-11-04 00:02] Guard #99 begins shift",
"[1518-11-04 00:36] falls asleep",
"[1518-11-04 00:46] wakes up",
"[1518-11-05 00:03] Guard #99 begins shift",
"[1518-11-05 00:45] falls asleep",
"[1518-11-05 00:55] wakes up")
  val input = lines// testLines

  def fill(s: Stream[Event]): Stream[Event] = {
    s match {
      case s if s.isEmpty => Stream.empty
      case Event(_, Some(id), BeginShift) #:: tail => {
        val (a, b) = tail.span(_.id.isEmpty)
        a.map(_.withId(id)) ++ fill(b)
      }
      case _ => throw new RuntimeException("xxx")
    }

  }

  val ordered = input.map(Event.parse).sortWith((x, y) => !x.when.isAfter(y.when)).toStream
  val filled = fill(ordered)

  def sumSleep(s: Stream[Event]): Long = {
    var fa: Option[LocalDateTime] = None
    var sum: Long = 0L
    for (e <- s) {
      e match {
        case Event(when, _, FallsAsleep) => fa = Some(when)
        case Event(when, _, WakesUp) => sum = sum + Duration.between(fa.get, when).toMinutes
        case _ => throw new RuntimeException("yyy")
      }
    }
    sum
  }

  def toSleepPeriods(s: Stream[Event]): Stream[Sleep] = {
    if (s.isEmpty)
      Stream.empty
    else {
      //val x = s.grouped(2).toStream
      //val (h, t) = (x.head.toList, x.tail)
      val (h, t) = s.splitAt(2)

      Stream.cons(Sleep.from(h.head, h(1)), toSleepPeriods(t))
    }
  }

  case class Sleep(s: LocalDateTime, e: LocalDateTime)
  {
    def durmin: Long = Duration.between(s, e).toMinutes
    def hasMin(min: Int) = s.getMinute <= min && min < e.getMinute
  }

  object Sleep
  {
    def from(fs: Event, wu: Event): Sleep = {
      assert(fs.t == FallsAsleep)
      assert(wu.t == WakesUp)

      Sleep(fs.when, wu.when)
    }
  }


  /**
    * Find which minute was most asleep and count of times
    *
    * @param s
    * @return (Minute, # of times asleep)
    */
  def minSleepOcc(s: Stream[Sleep]): (Int, Int) = Stream.range(0, 60).map(m => (m, s.count(_.hasMin(m)))).maxBy(_._2)

  // chronologicaly sorted event grouped by ID
  val byId = filled.groupBy(_.id)

  val biggestSleeperId = byId.map(group =>(group._1, sumSleep(group._2))).toStream.maxBy(_._2)._1
  println(s"Biggest sleeper's ID: $biggestSleeperId")

  val minute = minSleepOcc(toSleepPeriods(byId(biggestSleeperId)))._1
  val result1 = minute * biggestSleeperId.get
  println(s"result 1 : $result1")


  val mostAsleepOnTheSameMinute = byId
    .mapValues(toSleepPeriods) // id -> Stream[Sleep]
    .mapValues(minSleepOcc) // id -> (minute, #)
    .maxBy(_._2._2) // find max KV by V.#
  val result2 = mostAsleepOnTheSameMinute._2._1 * mostAsleepOnTheSameMinute._1.get
  println(s"result 2 : $result2")
}
