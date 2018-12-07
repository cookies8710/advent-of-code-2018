import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

object Main extends App {
  val lines = Files.readAllLines(Paths.get("input")).asScala

  case class Dependency(start: Char, end: Char)

  object Dependency {
    val pattern = "Step (.) must be finished before step (.) can begin.".r
    def apply(string: String): Dependency = string match { case pattern(start, end) => Dependency(start.head, end.head) }
  }

  val testLines = List(
    "Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin.")

  //val (input, workers, baseDuration) = (testLines, 2, 0)
  val (input, workers, baseDuration) = (lines, 5, 60)
  val dependencies = input.map(Dependency.apply).toStream

  def canProceed(done: String, dependencies: Stream[Dependency])(step: Char): Boolean = {
    assert(!done.contains(step))
    dependencies.filter(_.end == step)
      .map(_.start)
      .forall(x => done.contains(x.toString))
  }

  def step(done: String, notDone: Stream[Char], dependencies: Stream[Dependency]): (String, Stream[Char]) = {
    val candidates = notDone.filter(canProceed(done, dependencies))
    assert(candidates.nonEmpty)

    val next = candidates.sorted.head
    (done + next, notDone.filter(_ != next))
  }

  def construct(dps: Stream[Dependency]): String = {
    val steps = dps.flatMap(d => List(d.start, d.end)).distinct

    Stream.iterate("" -> steps)(x => step(x._1, x._2, dps)).find(_._2.isEmpty).get._1
  }

  val result1 = construct(dependencies)
  println(s"result 1 : $result1")

  case class Progress(step: Char, duration: Int) {
    def inced: Progress = Progress(step, duration + 1)
    def done: Boolean = (baseDuration + 1 + step.toInt - 'A'.toInt) <= duration
  }

  object Progress
  {
    def apply(step: Char): Progress = Progress(step, 0)
  }

  def stepTimed(workers: Int)(done: String, notDone: Stream[Char], progress: Set[Progress], time: Int, dependencies: Stream[Dependency]): (String, Stream[Char], Set[Progress], Int) = {
    assert(progress.size <= workers)
    val (finished, continuing) = progress.map(_.inced).partition(_.done)

    val newDone = done + new String(finished.map(_.step).toStream.sorted.toArray)

    val (candidates, notDoneCarry) = notDone.partition(canProceed(newDone, dependencies))
    val (newProgress, newNotDone) = candidates.sorted.splitAt(workers - continuing.size)

    (newDone, notDoneCarry ++ newNotDone, continuing ++ newProgress.map(Progress.apply), time + 1)
  }

  def constructTimed(workers: Int, dependencies: Stream[Dependency]): Int = {
    val steps = dependencies.flatMap(d => List(d.start, d.end)).distinct.sorted
    Stream.iterate(("", steps, Set[Progress](), 0))(tr => stepTimed(workers)(tr._1, tr._2, tr._3, tr._4, dependencies)).find(tr => tr._2.isEmpty && tr._3.isEmpty).get._4 - 1
  }

  val result2 = constructTimed(workers, dependencies)
  println(s"result 2 : $result2")

}
