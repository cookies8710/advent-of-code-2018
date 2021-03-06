import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

object Main extends App {
  val lines = Files.readAllLines(Paths.get("input")).asScala

  val testLines = List("1, 1",
    "1, 6",
    "8, 3",
    "3, 4",
    "5, 5",
    "8, 9")

  case class Coords(x: Int, y: Int)
  {
    def dist(other: Coords): Int = Math.abs(x - other.x)+ Math.abs(y - other.y)
  }
  object Coords
  {
    val pattern = "(\\d+),\\s+(\\d+)".r
    def apply(s: String): Coords = s match { case pattern(x, y) => Coords(Integer.parseInt(x), Integer.parseInt(y)) }
  }

  val input = lines
  val inputCoordinates = input.map(Coords.apply).toStream

  def getAreas(coords: Stream[Coords], ld: Coords, ru: Coords): Map[Int, Int] = {
    var areas: Map[Int, Int] = coords.indices.map(_ -> 0).toMap
    for (x <- ld.x to ru.x; y <- ld.y to ru.y)
    {
      val dists = coords.zipWithIndex.map(c => (c._2, c._1.dist(Coords(x, y))))
      val min = dists.map(_._2).min
      val closest = dists.filter(_._2 == min)
      if (closest.length == 1) {
        val k = closest.head._1
        areas = areas + (k -> (areas(k) + 1))
      }
    }

    areas
  }

  def getMaxNonInfinite(coords: Stream[Coords]): Int = {
    val xs = coords.map(_.x)
    val ys = coords.map(_.y)

    val (l, r) = (xs.min, xs.max)
    val (u, d) = (ys.max, ys.min)

    val indexes = coords.indices

    val a1 = getAreas(coords, Coords(l, d), Coords(r, u))
    val a2 = getAreas(coords, Coords(l - 1, d - 1), Coords(r + 1, u + 1))

    val finite = indexes.filter(i => a1(i) == a2(i)) // finite areas are those which do not extend
    a1(finite.maxBy(a1(_)))
  }

  val result1 = getMaxNonInfinite(inputCoordinates)
  println(s"result 1 = $result1")

  val limit = 10000
  def safeLocation(location: Coords, coords: Stream[Coords]): Boolean = coords.map(location.dist).sum < limit

  def safeRegion(coords: Stream[Coords]): Int = {
    val xs = coords.map(_.x)
    val ys = coords.map(_.y)

    val (l, r) = (xs.min, xs.max)
    val (u, d) = (ys.max, ys.min)

    var regionArea = 0
    for (x <- l to r; y <- d to u)
      {
       val loc = Coords(x, y)
        if (safeLocation(loc, coords)) regionArea = regionArea + 1
      }

    regionArea
  }

  val result2 = safeRegion(inputCoordinates)
  println(s"result 2 = $result2")
}
