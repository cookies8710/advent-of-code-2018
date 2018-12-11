import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

object Main extends App {
  val lines = Files.readAllLines(Paths.get("input")).asScala

  case class C2(x: Int, y: Int)
  object C2
  {
    def parse(s: String): C2 = {
      val x = s.split(",").map(_.trim).map(_.toInt).take(2)
      assert(x.size == 2)
      C2(x.head, x.last)
    }
  }

  case class Point(position: C2, velocity: C2)
  {
    def at(t: Int): Point = Point(C2(position.x + t * velocity.x, position.y + t * velocity.y), velocity)
  }
  object Point
  {
    val pat = "position=<([^>]+)> velocity=<([^>]+)>".r
    def parse(s: String): Point = s match {
      case pat(pos, vel) => Point(C2.parse(pos), C2.parse(vel))
    }
  }

  val testLines = List(
"position=< 9,  1> velocity=< 0,  2>",
"position=< 7,  0> velocity=<-1,  0>",
"position=< 3, -2> velocity=<-1,  1>",
"position=< 6, 10> velocity=<-2, -1>",
"position=< 2, -4> velocity=< 2,  2>",
"position=<-6, 10> velocity=< 2, -2>",
"position=< 1,  8> velocity=< 1, -1>",
"position=< 1,  7> velocity=< 1,  0>",
"position=<-3, 11> velocity=< 1, -2>",
"position=< 7,  6> velocity=<-1, -1>",
"position=<-2,  3> velocity=< 1,  0>",
"position=<-4,  3> velocity=< 2,  0>",
"position=<10, -3> velocity=<-1,  1>",
"position=< 5, 11> velocity=< 1, -2>",
"position=< 4,  7> velocity=< 0, -1>",
"position=< 8, -2> velocity=< 0,  1>",
"position=<15,  0> velocity=<-2,  0>",
"position=< 1,  6> velocity=< 1,  0>",
"position=< 8,  9> velocity=< 0, -1>",
"position=< 3,  3> velocity=<-1,  1>",
"position=< 0,  5> velocity=< 0, -1>",
"position=<-2,  2> velocity=< 2,  0>",
"position=< 5, -2> velocity=< 1,  2>",
"position=< 1,  4> velocity=< 2,  1>",
"position=<-2,  7> velocity=< 2, -2>",
"position=< 3,  6> velocity=<-1, -1>",
"position=< 5,  0> velocity=< 1,  0>",
"position=<-6,  0> velocity=< 2,  0>",
"position=< 5,  9> velocity=< 1, -2>",
"position=<14,  7> velocity=<-2,  0>",
"position=<-3,  6> velocity=< 2, -1>" )

  case class Box(l: Int, r: Int, u: Int, d: Int)// y is up negative, down positive
  {
    def area: Int = (r - l) * (d - u)
  }

  object Box
  {
    def apply(points: List[Point]): Box = {
      val xs = points.map(_.position.x)
      val ys = points.map(_.position.y)

      val (l, r) = (xs.min, xs.max)
      val (d, u) = (ys.max, ys.min)
      Box(l, r, u, d)
    }
  }


  def render(points: List[Point]): Unit = {
    Box(points) match {
      case Box(l, r, u, d) =>

        for(y <- u to d; x <- l to r)
        {
          if (x == l) println()
          if (points.exists {case Point(pos, _) => pos.x == x && pos.y == y})
            print('#') else print('.')
        }
        println()
    }
  }

  def at(t: Int, points: Stream[Point]): Stream[Point] = points.map(_.at(t))

  val input = lines // testLines
  val interval = 10000 to 11000 // from looking at the input, it's safe to skip time by at least 10000 seconds
  // smallest area: T(10243): area = 549

  val pts = input.map(Point.parse)
  /*interval.foreach {time =>
    println(s"Time $time:")
    render(at(time, pts.toStream).toList)
    Thread.sleep(1000)
  }*/

  //val boxes = interval.map(at(_, pts.toStream)).map(x => Box(x.toList)).map(_.area).zip(interval)
   // .foreach { case (area, time) => println(s"T($time): area = $area")}


  render(at(10243, pts.toStream).toList)
}
