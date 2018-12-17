import scala.collection.mutable

object Main extends App {
  abstract class TileType
  object Clay extends TileType
  object Sand extends TileType
  object WetSand extends TileType
  object Water extends TileType
  object Spring extends TileType

  val TileTypeCharacters = Map(Clay -> '#', Sand -> '.', WetSand -> '|', Water -> '~', Spring -> '+')

  case class Coordinates(x: Int, y: Int)
  {
    def up: Coordinates = Coordinates(x, y - 1)
    def down: Coordinates = Coordinates(x, y + 1)
    def left: Coordinates = Coordinates(x - 1, y)
    def right: Coordinates = Coordinates(x + 1, y)
  }

  implicit def tupleToCoords(t: (Int, Int)): Coordinates = Coordinates(t._1, t._2)
  val realInput = scala.io.Source.fromFile("input").getLines().toList
  val testInput = List(
    "x=495, y=2..7",
    "y=7, x=495..501",
    "x=501, y=3..7",
    "x=498, y=2..4",
    "x=506, y=1..2",
    "x=498, y=10..13",
    "x=504, y=10..13",
    "y=13, x=498..504")
  val testInput2 = List(
    "x=495, y=10..20",
    "y=20, x=495..515",
    "x=515, y=10..20",
    "x=505, y=12..16",
    "x=507, y=12..16",
    "x=506, y=16")
  val input = realInput

  class State(minY: Int, maxY: Int)
  {
    val SpringCoordinates: Coordinates = 500 -> 0
    val map: mutable.Map[Coordinates, TileType] = mutable.Map().withDefaultValue(Sand).updated(SpringCoordinates, Spring)

    def countWet: Int = map.filter {case (Coordinates(_, y), _) => minY <= y && y <= maxY }.values.count(_ == WetSand)
    def countWater: Int = map.filter {case (Coordinates(_, y), _) => minY <= y && y <= maxY }.values.count(_ == Water)
    def peekDown(coords: Coordinates): TileType = map(coords.down)

    private def wet(coords: Coordinates): Unit = map.update(coords, WetSand)
    private def water(coords: Coordinates): Unit =  map.update(coords, Water)
    private def solid(tile: TileType): Boolean = Set(Clay, Water).contains(tile)

    // mutates state
    def pour(from: Coordinates): Unit = {
      // go down until hit clay, water or too large y
      var ptr = from
      while(peekDown(ptr) == WetSand)
        ptr = ptr.down
      while (!solid(peekDown(ptr)) && ptr.y < maxY)
      {
        ptr = ptr.down
        wet(ptr)
      }

      if (ptr.y < maxY) {
        // find edge in selected direction; first part of result is whether the edge is solid (can contain water), the other is edge position
        def findEdge(coords: Coordinates, move: Coordinates => Coordinates): (Boolean, Coordinates) = {
          var current = coords
          while (solid(peekDown(current)) && !solid(map(current)))
          {
            wet(current)
            current = move(current)
          }
          (solid(peekDown(current)), current)
        }

        val (leftSolidEdge, left) = findEdge(ptr, _.left)
        val (rightSolidEdge, right) = findEdge(ptr, _.right)

        // if there are solid edges on both sides, water fills the row
        if (leftSolidEdge && rightSolidEdge)
          (left.x + 1 to right.x - 1).foreach(x => water(x -> ptr.y))
        else {
          // otherwise at least one of the edges is not solid and water spills over the edge
          if (!rightSolidEdge) {
            wet(right)
            pour(right)
          }
          if (!leftSolidEdge) {
            wet(left)
            pour(left)
          }
        }
      }
    }

    def render(): Unit = {
      println()
      def minMax(l: Iterable[Int]): (Int, Int) = (l.min, l.max)

      val (xa, xb) = minMax(map.keys.map(_.x))
      val (ya, yb) = minMax(map.keys.map(_.y))
      for (y <- ya to yb; x <- xa to xb)
      {
        if (x == xa)
          print(f"\n$y%4d: ")
        print(TileTypeCharacters(map(x -> y)))
      }
    }
  }

  object State
  {
    abstract class Value
    {
      def list: List[Int]
      def min: Int
      def max: Int
    }

    case class Scalar(n: Int) extends Value
    {
      override def list: List[Int] = List(n)
      override def min: Int = n
      override def max: Int = n
    }

    case class Interval(a: Int, b: Int) extends Value
    {
      override def list: List[Int] = (a to b).toList
      override def min: Int = a
      override def max: Int = b
    }

    private val scalar = "(\\d+)".r
    private val interval = "(\\d+)\\.\\.(\\d+)".r

    def getSegment(assignments: String): (Value, Value) = {
      // get two assignments, ordered lexicographically
      val xy = assignments.split(',').map(_.trim).sorted

      // converts assignment to Value; drop coordinate e.g. x=1 -> Scalar(1), y=1..10 -> Interval(1, 10)
      def e2v(es: String): Value = es.split('=').last match {
        case scalar(n) => Scalar(n.toInt)
        case interval(a, b) => Interval(a.toInt, b.toInt)
      }

      (e2v(xy.head), e2v(xy.last))
    }

    def load(inputRows: List[String]): State = {
      val clayTiles = inputRows.map(getSegment)
      val miny = clayTiles.map(_._2.min).min
      val maxy = clayTiles.map(_._2.max).max
      val state = new State(miny, maxy)
      clayTiles.foreach { case (xvals, yvals) =>
        for (x <- xvals.list; y <- yvals.list)
          state.map.update(x -> y, Clay)
      }
      state
    }
  }

  val state = State.load(input)
  var water = state.countWater
  var wet = state.countWet
  var end = false
  while (!end)
  {
    state.pour(state.SpringCoordinates)
    val newWater = state.countWater
    val newWet = state.countWet
    end = newWater == water && newWet == wet
    water = newWater
    wet = newWet
  }
  state.render()
  println()
  println(s"$water tiles contain water, $wet tiles are contain wet send - in total ${water + wet} tiles were reached by water")
}
