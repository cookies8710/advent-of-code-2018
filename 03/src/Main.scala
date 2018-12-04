import java.nio.file.{Files, Path, Paths}
import java.util.regex.Pattern

import scala.collection.JavaConverters._

object Main extends App {

  case class Segment(s: Int, e: Int)
  {
    def overlap(other: Segment): Boolean = !(s > other.e || other.s > e)
  }

  case class Claim(claim: Int, x: Int, y: Int, w: Int, h: Int)
  {
    def segs: (Segment, Segment) = (Segment(x, x + w -1), Segment(y, y + h - 1))
    def hasPoint(dx: Int, dy: Int): Boolean = x <= dx && dx < (x+w) && y <= dy && dy < (y+h)
    def overlap(other: Claim): Boolean = {
      val ((tx, ty), (ox, oy)) = (segs, other.segs)
      tx.overlap(ox) && ty.overlap(oy)
    }
  }

  object Claim
  {
    val pattern = Pattern.compile("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$")
    def apply(s: String): Option[Claim] = {
      val m = pattern.matcher(s)
      if (m.matches())
        Some(Claim(Integer.parseInt(m.group(1)), Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)), Integer.parseInt(m.group(4)), Integer.parseInt(m.group(5))))
      else
        None
    }
  }

  val lines = Files.readAllLines(Paths.get("input")).asScala
    .map(Claim.apply)
  val max = (lines.map(t => t.get.x + t.get.w).max, lines.map(t => t.get.y + t.get.h).max)
  var c = 0
  for(i <- Stream.range(0, max._1 + 1);
      j <- Stream.range(0, max._1 + 1))
  {
    val m =lines.filter(_.get.hasPoint(i, j)).take(2).toList
    if (m.length == 2)c = c + 1
  }

  println(c)

  val flattened = lines.flatten
  val result2 = flattened.find(o => flattened.forall(p => p == o || !p.overlap(o)))
  println(result2)

}
