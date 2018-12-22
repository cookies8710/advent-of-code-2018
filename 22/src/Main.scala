import scala.collection.mutable

object Main extends App {
  val testInput = (10 -> 10, 510)
  val realInput = (8 -> 701, 5913)

  trait TileType
  {
    override def toString: String = this.getClass.getSimpleName.filter(_.isLetter)
  }
  object Rocky extends TileType
  object Wet extends TileType
  object Narrow extends TileType

  case class Tile(geologicalIndex: Int)
  {
    def erosionLevel(depth: Int): Int = (depth + geologicalIndex) % 20183
    def risk(depth: Int): Int = erosionLevel(depth) % 3
    def tileType(depth: Int): TileType = risk(depth) match {
      case 0 => Rocky
      case 1 => Wet
      case 2 => Narrow
    }
  }

  trait Tool
  {
    override def toString: String = this.getClass.getSimpleName.filter(_.isLetter)
  }
  object Torch extends Tool
  object ClimbingGear extends Tool
  object Neither extends Tool

  val ValidTools = Map(Rocky -> Set(Torch, ClimbingGear), Wet -> Set(ClimbingGear, Neither), Narrow -> Set(Torch, Neither))

  class Cave(target: (Int, Int), depth: Int)
  {
    val ext = 45
    val cave: mutable.Map[(Int, Int), Tile] = mutable.Map()
      .updated(0 -> 0, Tile(0))
      .updated(target, Tile(0))

    for (x <- 0 to (target._1 + ext))
      cave.update(x -> 0, Tile(x * 16807))
    for (y <- 0 to (target._2 + ext))
      cave.update(0 -> y, Tile(y * 48271))

    def totalRisk: Int = cave
      .filter { case ((x, y), _) => 0 <= x && x <= target._1 && 0 <= y && y <= target._2 }
      .values.map(_.risk(depth)).sum

    def explore(): Boolean = {
      var updated = false
      for (y <- 1 to (target._2 + ext); x <- 1 to (target._1 + ext))
      {
        val left = (x - 1) -> y
        val up = x -> (y - 1)
        if (!cave.contains(x -> y) && cave.contains(left) && cave.contains(up))
        {
          cave.update(x -> y, Tile(cave(left).erosionLevel(depth) * cave(up).erosionLevel(depth)))
          updated = true
        }
      }
      updated
    }

    def targetRescue: (Int, List[(Int, Int, Tool)]) = rescuePaths(target)(Torch)
    def printRescuePaths(): Unit = {
      for (y <- 0 to (target._2 + ext); x <- 0 to (target._1 + ext))
        println(s"[$x, $y]: ${rescuePaths(x -> y)}")
    }
    // rescue paths: Map[(Int,Int), Map[Tool, Int]] -> position -> (tool -> fastest path)
    val rescuePaths: mutable.Map[(Int,Int), Map[Tool, (Int, List[(Int,Int,Tool)])]] =
      mutable.Map().updated(0 -> 0, Map(Torch -> (0, List((0,0,Torch))), ClimbingGear -> (7, List((0,0,Torch)))))
    def prices(currentTool: Tool, tgttools: Set[Tool]): Map[Tool, Int] = tgttools.map(t => t -> (if (t == currentTool) 0 else 7)).toMap
    def buildRescuePaths(): Boolean = {
      var updated = false
      val maxDetour = ext - 10
      for (y <- 0 to (target._2 + maxDetour); x <- 0 to (target._1 + maxDetour))
      {
        val currentPos = x -> y
        val left = (x - 1) -> y
        val right = (x + 1) -> y
        val up = x -> (y - 1)
        val down = x -> (y + 1)
        // if at least one tool for curr pos: for each dir: for each current tool:
        //      if the target permits tool and the new price is lower or first, updat
        if (rescuePaths.contains(currentPos))
        {
          for (newPosition <- Set(left, right, up, down);
               tool <- rescuePaths(currentPos).keys if newPosition._1 >= 0 && newPosition._2 >= 0) {
            val targetTile: Tile = cave(newPosition)
            val targetTools: Set[Tool] = ValidTools(targetTile.tileType(depth))
            val toolPrices: Map[Tool, Int] = prices(tool, targetTools)
            for ((targetTool, targetToolPrice) <- toolPrices)
            {
              val newPrice: Int = 1 + rescuePaths(currentPos)(tool)._1 + targetToolPrice // 1 for step, current tool current pos price, target tool price
              val cp = (newPosition._1, newPosition._2, targetTool) :: rescuePaths(currentPos)(tool)._2
              if (!rescuePaths.contains(newPosition))
              {
                val ti: Map[Tool, (Int, List[(Int, Int, Tool)])] = Map(targetTool -> (newPrice, cp))
                rescuePaths.update(newPosition, ti)
                updated = true
              }
              else if(!rescuePaths(newPosition).contains(targetTool) || rescuePaths(newPosition)(targetTool)._1 > newPrice)
              {
                rescuePaths(newPosition) = rescuePaths(newPosition).updated(targetTool, (newPrice, cp))
                updated = true
              }
            }
          }
        }
      }
      updated
    }
    //def rescue(pos: (Int, Int))

    def render(): Unit = {
      for (y <- 0 to target._2; x <- 0 to target._1)
        {
          if (x == 0)
            println()
          val tt = cave(x -> y).tileType(depth) match {
            case Rocky => '.'
            case Wet => '='
            case Narrow => '|'
          }
          print(tt)
        }
      println()
    }
  }

  val input = realInput//testInput
  val c = new Cave(input._1, input._2)
  while(c.explore()){}
  c.render()
  println(s"Total risk: ${c.totalRisk}")

  // part 2
  var i = 0
  while(c.buildRescuePaths()) {}

  val (result2, path) = c.targetRescue
  println(s"Target rescue path:")
  path.reverse.foreach(println)
  println(s"Target rescue price: $result2")
  var price = 0
  var pt = path.reverse.head._3
  for (ns <- path.reverse)
    {
      price = price + (if (pt == ns._3) 1 else 8)
      pt = ns._3
    }
  println(s"Recalculated rescue price: $price")
}
