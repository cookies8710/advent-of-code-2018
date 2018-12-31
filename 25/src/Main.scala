object Main extends App {
  val testInput1 = scala.io.Source.fromFile("test-input-1").getLines.toList
  val testInput2 = scala.io.Source.fromFile("test-input-2").getLines.toList
  val testInput3 = scala.io.Source.fromFile("test-input-3").getLines.toList

  val realInput = scala.io.Source.fromFile("input").getLines.toList

  case class Star(x: Int, y: Int, z: Int, w: Int)
  {
    def distance(o: Star): Int = Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z) + Math.abs(w - o.w)
  }

  object Star
  {
    def parse(s: String): Star = {
      val parts = s.split(",").map(_.toInt)
      Star(parts(0), parts(1), parts(2), parts(3))
    }
  }

  class Constellation(val stars: Set[Star])
  {
    def distance(s: Star): Int = stars.map(_.distance(s)).min
    def partition(s: Set[Star]): (Set[Star], Set[Star]) = s.partition(distance(_) <= 3)
  }

  def findConstellations(stars: Set[Star]): Set[Constellation] = {
    var constellations: Set[Constellation] = Set()
    var freeStars: Set[Star] = stars
    var currentConstellation = new Constellation(Set())

    while(freeStars.nonEmpty)
    {
      val (constellationSeed, rest) = freeStars.splitAt(1)

      freeStars = rest
      currentConstellation = new Constellation(constellationSeed)
      var end = false
      while(!end)
      {
        val (constellationPart, newFreeStars) = currentConstellation.partition(freeStars)
        currentConstellation = new Constellation(currentConstellation.stars ++ constellationPart)
        end = newFreeStars.size == freeStars.size
        freeStars = newFreeStars
      }
      constellations = constellations + currentConstellation
    }

    constellations
  }

  val input = realInput
  val allStars = input.map(Star.parse).toSet
  val constellations = findConstellations(allStars)

  println(s"${constellations.size} constellations found")
}