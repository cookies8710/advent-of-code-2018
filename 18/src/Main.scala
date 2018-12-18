object Main extends App {
  trait TileType
  object Trees extends TileType
  object Lumberyard extends TileType
  object Open extends TileType

  val TileToChar: Map[TileType, Char] = Map(Open -> '.', Trees -> '|', Lumberyard ->  '#')
  val CharToTile: Map[Char, TileType] = TileToChar.map { case (k, v) => v -> k }

  type Coordinates = (Int, Int)

  val input = scala.io.Source.fromFile("input").getLines().toList

  case class State(state: Map[Coordinates, TileType])
  {
    def resourceVal = state.values.count(_ == Trees) * state.values.count(_ == Lumberyard)

    val (xa, xb) = (state.keySet.map(_._1).min, state.keySet.map(_._1).max)
    val (ya, yb) = (state.keySet.map(_._2).min, state.keySet.map(_._2).max)

    def render(): Unit = {
      for(y<-ya to yb; x<-xa to xb)
      {
        if (x == xa)
          println()
        print(TileToChar(state(x->y)))
      }

      println(s"\nResource value: $resourceVal")
    }

    def nextMinute(): State = {
      def tileAt(c: Coordinates): Option[TileType] =
        if (c._2 >= ya && c._2 <= yb && c._1 >= xa && c._1 <= xb) Some(state(c)) else None
      def surroundingIndices(c: Coordinates): List[Coordinates] =
        (for(dx <- -1 to 1; dy <- -1 to 1) yield dx -> dy)
          .filter { case (dx, dy) => dx != 0 || dy != 0 }
          .map { case (dx, dy) => (c._1 + dx, c._2 + dy)}.toList
      def surroundingTiles(c: Coordinates): List[TileType] = surroundingIndices(c).flatMap(tileAt)

      def tileNextMinute(tile: TileType, surrounding: List[TileType]): TileType = tile match {
        case Open => if (surrounding.count(_ == Trees) >= 3) Trees else Open
        case Trees => if (surrounding.count(_ == Lumberyard) >= 3) Lumberyard else Trees
        case Lumberyard => if (surrounding.contains(Lumberyard) && surrounding.contains(Trees)) Lumberyard else Open
      }

      val nextState = (for(y <- ya to yb; x<-xa to xb)
        yield (x -> y) -> tileNextMinute(state(x -> y), surroundingTiles(x -> y))).toMap

      State(nextState)
    }
  }

  object State
  {
    def parse(lines: List[String]): State = {
      val state: Map[Coordinates, TileType] =
        lines
          .zipWithIndex
          .flatMap { case (line, y) =>
            line
              .zipWithIndex.map { case (char, x) =>
              (x -> y, CharToTile(char))}}
          .toMap
      State(state)
    }
  }

  val part1 = 10

  // period 28
  // (10**9 - 1500) % 28 = 4 => same score as in minute 1504
  val part2 = 1504//1000000000

  var state = State.parse(input)
  var unique: Map[Int, Int] = Map()
  for(minute <- 1 to part1)
  {
    state = state.nextMinute()
    val resourceVal = state.resourceVal
    val last = if (unique.contains(resourceVal)) Some(unique(resourceVal)) else None
    unique = unique.updated(resourceVal, minute)
    print(s"\nAfter $minute minutes: ${state.resourceVal}, (#U: ${unique.keySet.size}), last seen: ${last.map(n => s"${minute - n} minutes ago").getOrElse("UNIQUE")}")
  }
  state.render()
}
