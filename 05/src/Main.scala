import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

object Main extends App {
  val lines = Files.readAllLines(Paths.get("input")).asScala
  val testLines = List("dabAcCaCBAcCcaDA")

  val input = lines(0)
  val types = input.toLowerCase.sorted.distinct
  val reactants = types.flatMap(t => List(s"$t${t.toUpper}", s"${t.toUpper}$t"))
  def react(in: String): (String, Boolean) = {
    val reacted = reactants.fold(in)((str, reactant) => str.replaceAll(reactant, ""))
    (reacted, reacted == in)
  }
  def fullyReact(s: String): String = Stream.iterate((s, false))(t => react(t._1)).find(_._2).map(_._1).get
  val result1 = fullyReact(input).length
  println(s"result 1 = $result1")

  val result2 = types.map(t => input.replaceAll(s"${t.toUpper}", "").replaceAll(s"$t", "")).map(fullyReact).map(_.length).min
  println(s"result 2 = $result2")
}
