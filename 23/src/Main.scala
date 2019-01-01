object Main extends App {
  type CoordinateType = Long

  case class Coordinates(x: CoordinateType, y: CoordinateType, z: CoordinateType)
  {
    def distance(o: Coordinates): CoordinateType = Math.abs(x - o.x) + Math.abs(y - o.y) + Math.abs(z - o.z)
    def /(factor: CoordinateType): Coordinates = Coordinates(x / factor, y / factor, z / factor)
    def +(o: Coordinates): Coordinates = Coordinates(x + o.x, y + o.y, z + o.z)
    def *(factor: CoordinateType): Coordinates = Coordinates(x * factor, y * factor, z * factor)
    lazy val length: CoordinateType = distance(Coordinates.Origin)
  }

  object Coordinates
  {
    val Origin = Coordinates(0, 0, 0)
  }

  case class Nanobot(position: Coordinates, r: CoordinateType)
  {
    //def overlaps(o: Nanobot): Boolean = position.distance(o.position) <= r + o.r
    //def overlaps(g: Set[Nanobot]): Boolean = g.forall(this.overlaps)

    def /(factor: CoordinateType): Nanobot = if (factor == 1) Nanobot(position, r) else {
      val nr = r / factor + 2 // enlarge the radius to compensate for precision loss; the assumption is that in coarser levels, there should be all overlaps from finer levels
      Nanobot(position / factor, nr)
    }
    def contains(point: Coordinates): Boolean = position.distance(point) <= r
  }

  object Nanobot
  {
    val nanobotRegex = "pos=<([-\\d]+),([-\\d]+),([-\\d]+)>, r=(\\d+)".r
    def parse(s:String):Nanobot = s match {
      case nanobotRegex(xs, ys, zs, rs) => Nanobot(Coordinates(xs.toLong, ys.toLong, zs.toLong), rs.toLong)
    }
  }

  val testInput1 = scala.io.Source.fromFile("test-input-1").getLines().toList
  val testInput2 = scala.io.Source.fromFile("test-input-2").getLines().toList
  val realInput = scala.io.Source.fromFile("input").getLines().toList

  val input = realInput
  val nanobots: List[Nanobot] = input.map(Nanobot.parse)
  val strongest = nanobots.maxBy(_.r)
  val result1 = nanobots.count(o => strongest.position.distance(o.position) <= strongest.r)
  println(s"result1: $result1")

  case class Volume(a: Coordinates, b: Coordinates)
  {
    def generatePoints: IndexedSeq[Coordinates] = for (z <- a.z to b.z; y <- a.y to b.y; x <- a.x to b.x) yield Coordinates(x, y, z)
    val size = (b.x - a.x) * (b.y - a.y) * (b.z - a.z)
  }

  def getFullVolume(n: List[Nanobot]): Volume = {
    def minMax(l: List[CoordinateType]): (CoordinateType, CoordinateType) = (l.min, l.max)
    val (xa, xb) = minMax(n.map(_.position.x))
    val (ya, yb) = minMax(n.map(_.position.y))
    val (za, zb) = minMax(n.map(_.position.z))
    Volume(Coordinates(xa, ya, za), Coordinates(xb, yb, zb))
  }

  def findMaxes(n: List[Nanobot], volume: Volume): List[(Int, Coordinates)] = {
    var res = Map[Coordinates, Int]().withDefaultValue(0)
    for (coo <- volume.generatePoints; nanobot <- n)
    {
      if (nanobot.contains(coo))
        res = res.updated(coo, res(coo) + 1)
    }
    res.toList.sortWith((a, b) => a._2 > b._2 || (a._2 == b._2 && a._1.length < b._1.length)).map(_.swap)
  }

  var cellsToExplore: List[(Int, Int, Coordinates)] = List((9, 1000, Coordinates.Origin)) // factor, # overlaps, cell

  val base = 10
  var result2: Option[(Int, Coordinates)] = None // current best; # overlaps, coords
  var end = false
  while (!end)
  {
    val (current, rest) =
      cellsToExplore.sortWith {
        case ((fa, oa, cooa), (fb, ob, coob)) if oa != ob => oa > ob
        case ((fa, oa, cooa), (fb, ob, coob)) if oa == ob => fa > fb
        case ((fa, oa, cooa), (fb, ob, coob)) if fa == fb && oa == ob => cooa.length < coob.length
      } match {
        case h :: t => (h, t)
      }
    val exponent = current._1 - 1
    val factor = Math.pow(base, exponent).toLong
    val smaller = nanobots.map(_ / factor)
    val volume = Some(current._3).map(cell => Volume(cell.*(base), cell.*(base).+(Coordinates(base, base, base)))).getOrElse(getFullVolume(smaller))
    val maxes = findMaxes(smaller, volume)
    val toadd: List[(Int,Int,Coordinates)] = maxes
      .takeWhile { case (overlaps, coord) => overlaps == maxes.head._1 && (result2.isEmpty || overlaps >= result2.get._1) }
      .map { case (overlaps, coord) => (exponent, overlaps, coord) }

    if (exponent == 0 && (result2.isEmpty || maxes.head._1 > result2.get._1 || (maxes.head._1 == result2.get._1 && maxes.head._2.length < result2.get._2.length)))
        result2 = Some(maxes.head)

    cellsToExplore = rest
    if (exponent > 0)
      cellsToExplore = cellsToExplore ++ toadd

    if (result2.isDefined)
      cellsToExplore = cellsToExplore.filter { case (_, o, _) => o >= result2.get._1}

    end = cellsToExplore.isEmpty
  }

  println(s"result2: $result2, dist: ${result2.map(_._2.length).getOrElse(-1)}")
}
