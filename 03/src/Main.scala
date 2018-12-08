import java.nio.file.{Files, Paths}

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
    val pattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$".r
    def apply(s: String): Option[Claim] = {
      s match {
        case pattern(claim, x, y, w, h) => Some(Claim(
          Integer.parseInt(claim),
          Integer.parseInt(x),
          Integer.parseInt(y),
          Integer.parseInt(w),
          Integer.parseInt(h)))
        case _ => None
      }
    }
  }

  val lines = Files.readAllLines(Paths.get("input")).asScala
    .map(Claim.apply)
  val max = (lines.map(t => t.get.x + t.get.w).max, lines.map(t => t.get.y + t.get.h).max)
  var c = 0
  for(i <- 0 to max._1;
      j <- 0 to max._1)
  {
    val m =lines.filter(_.get.hasPoint(i, j)).take(2).toList
    if (m.length == 2)c = c + 1
  }

  println(c)

  val flattened = lines.flatten
  val result2 = flattened.find(o => flattened.forall(p => p == o || !p.overlap(o)))
  println(result2)

}
