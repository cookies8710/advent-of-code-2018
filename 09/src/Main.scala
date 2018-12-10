import java.nio.file.{Files, Paths}
import scala.collection.JavaConverters._

object Main extends App {
  val lines = Files.readAllLines(Paths.get("input")).asScala

  case class Field(marbles: List[Int], inc: (Int, Int) => Unit) // current marble is head
  {
    def addMarble(m: Int): Field = {
      if (m % 1000 == 0) println(s"Adding $m ${100.0 * m/7130700.0} %")
      if (m % 23 == 0 && m > 0) {
        inc(m, m)

        val (h, t) = marbles.reverse.splitAt(6)
        inc(m, t.head)

        //println(s"addinc ${t.head + m}")

        Field(h.reverse ++ t.tail.reverse, inc)
      } else Field(m :: marbles.drop(2) ++ marbles.take(2), inc)
    }
  }


  //val (players, lastMarble) = (9, 25)
  //val (players, lastMarble) = (10, 1618) // shouldBe 8317
  //val (players, lastMarble) = (17, 1104) // shouldBe 2764
  //val (players, lastMarble) = (458, 71307)
  val (players, lastMarble) = (458, 7130700)


  def play(players: Int, lastMarble: Int): Int = {
    var scores = (0 until players).map(_ -> 0).toMap

    def inc(pl: Int, v: Int): Unit = {
      val player = (pl - 1) % players
      //println(s"inc: $player + $v")
      scores = scores.updated(player, v + scores(player))
    }

    (1 to lastMarble).foldLeft(Field(List(0), inc)) { case (f, m) => f.addMarble(m) }
    val (player, highScore) = scores.maxBy(_._2)
    println(s"High score: $highScore, player: ${player + 1}")
    println(s"total: ${scores.values.sum}")
    scores.values.sum
  }

  //(1 to 1618).map(play(1, _)).sliding(2).map(s => (s.head, s.last)).map { case (a, b) => b - a }.filter(_ != 0)
   //   .foreach(println)
  play(players, lastMarble)

}
