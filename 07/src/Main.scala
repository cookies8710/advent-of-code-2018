import java.nio.file.{Files, Paths}
import java.util.regex.Pattern

import scala.collection.JavaConverters._

object Main extends App {
  val lines = Files.readAllLines(Paths.get("input")).asScala

  case class Dependency(a: Char, b: Char)

  object Dependency {
    val pattern = Pattern.compile("Step (.) must be finished before step (.) can begin.")

    def apply(s: String): Dependency = {
      val m = pattern.matcher(s)
      assert(m.matches())
      Dependency(m.group(1).head, m.group(2).head)
    }
  }

  val testLines = List(
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin.")

  val input = lines
  val limit = 5 // 2 for test lines
  val baseLineDuration = 60 // 0 for test lines

  val deps = input.map(Dependency.apply).toStream

  def canProceed(done: String, d: Stream[Dependency])(c: Char): Boolean = {
    assert(!done.contains(c))
    d.filter(_.b == c)
      .map(_.a)
      .forall(x => done.contains(x.toString))
  }

  def step(done: String, todo: Stream[Char], d: Stream[Dependency]): (String, Stream[Char]) = {
    val cand = todo.filter(canProceed(done, d))
    assert(cand.nonEmpty)

    val next = cand.sorted.head
    (done + next, todo.filter(_ != next))
  }

  def construct(dps: Stream[Dependency]): String = {
    val steps = dps.flatMap(d => List(d.a, d.b)).distinct

    Stream.iterate("" -> steps)(x => step(x._1, x._2, dps)).find(_._2.isEmpty).get._1
  }

  // println(construct(deps))

  case class Progress(c: Char, dur: Int) {
    def inced: Progress = Progress(c, dur + 1)
    def done: Boolean = (baseLineDuration + 1 + c.toInt - 'A'.toInt) <= dur
  }

  object Progress
  {
    def apply(c: Char): Progress = Progress(c, 0)
  }

  def timedconstructstep(limit: Int)(done: String, todo: Stream[Char], progress: Set[Progress], time: Int, dps: Stream[Dependency]): (String, Stream[Char], Set[Progress], Int) = {
    println(s"#$time\tdone: $done, todo: ${todo.toList}, progress: $progress")
    assert(progress.size <= limit)
    val (finished, cnt) = progress.map(_.inced).partition(_.done)

    val done2 = done + new String(finished.map(_.c).toStream.sorted.toArray)

    val (candidates, todocarry) = todo.partition(canProceed(done2, dps))
    val (newprog, newtodo) = candidates.sorted.splitAt(limit - cnt.size)

    (done2, todocarry ++ newtodo, cnt ++ newprog.map(Progress.apply), time + 1)
  }

  def constructTimed(limit: Int, dps: Stream[Dependency]): String = {
    val steps = dps.flatMap(d => List(d.a, d.b)).distinct.sorted
    var done = ""

    Stream.iterate(("", steps, Set[Progress](), 0))(tr => timedconstructstep(limit)(tr._1, tr._2, tr._3, tr._4, dps)).find(tr => tr._2.isEmpty && tr._3.isEmpty).get._1
  }

  val result2 = constructTimed(limit, deps)
  println(s"result 2 : $result2")

}
