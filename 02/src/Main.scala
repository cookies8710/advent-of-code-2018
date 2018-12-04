import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

object Main extends App {
  val lines = Files.readAllLines(Paths.get("input")).asScala

  def hasn(n: Int)(s: String): Boolean = if (s.isEmpty) false else {
    val (x, y) = s.span(_ == s.head)
    x.length == n || hasn(n)(y)
  }

  def cnt(l: String): (Int, Int) = {
    val x = l.sorted
    val two = if (hasn(2)(x)) 1 else 0
    val three = if (hasn(3)(x)) 1 else 0
    val r = (two, three)
    r
  }


  val (x2, x3) = lines.map(cnt).unzip
  val result1 = x2.sum * x3.sum
  println(result1)

  // ======================================
  def near(s1: String, s2: String, t: Int): Boolean = {
    if (s1.isEmpty && s2.isEmpty)
      true
    else {
      if (s1.head == s2.head) near(s1.tail, s2.tail, t)
      else if (t > 0)near(s1.tail, s2.tail, t - 1)
      else false
    }

  }
  def near(s1: String, s2: String): Boolean = near(s1, s2, 1)

  val nl = lines.length
  for (i <- Stream.range(0, nl);
       j <- Stream.range(i + 1, nl))
  {
    if (near(lines(i), lines(j)))
      println(s"${lines(i)}\n${lines(j)}")
  }
}
