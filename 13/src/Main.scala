object Main extends App {
  val lines = scala.io.Source.fromFile("input").getLines.toList
  val input = lines

  abstract class NextTurn
  object Left extends NextTurn
  object Straight extends NextTurn
  object Right extends NextTurn
  val turns: Map[NextTurn, NextTurn] = Map(Left -> Straight, Straight -> Right, Right -> Left)
  case class Cart(x:Int,y:Int, heading: Int, nextTurn: NextTurn)
  {
    def samePos(c2: Cart): Boolean = x == c2.x && y == c2.y
    def charactes: Char = Cart.cartChars(heading)
  }

  object Cart
  {
    val cartChars = "^>v<"
    def getCarts(y:Int,s:String): List[Cart] =
      s.zipWithIndex
        .filter{case (c, _) => cartChars.contains(c)}
        .map { case (c, x) => Cart(x, y, cartChars.indexOf(c), Left)}
        .toList

    def clear(c: Char): Char = c match {
      case '^' => '|'
      case '>' => '-'
      case '<' => '-'
      case 'v' => '|'
      case c => c
    }
  }

  val cartsStart = input.zipWithIndex.flatMap{case (s, y)=>Cart.getCarts(y, s)}
  val linesClean = input.map(_.map(Cart.clear))

  case class Crash(x: Int, y: Int)

  def tick(carts: List[Cart], map: List[String]): (List[Cart], List[Crash]) = {
    def sortCarts(carts: List[Cart]): List[Cart] =
      carts.sortWith((a, b)=> a.y < b.y || (a.y == b.y && a.x < b.x))

    val sortedCarts = sortCarts(carts)

    def np(c: Cart): Cart = {
      val (nx, ny) = c.heading match {
        case 0 => (c.x, c.y - 1)
        case 1 => (c.x + 1, c.y)
        case 2 => (c.x, c.y + 1)
        case 3 => (c.x - 1, c.y)
      }

      val mapChar = map(ny)(nx)

      val (nh, nnt) = mapChar match {
        case '+' => c.nextTurn match {
          case Left => ((c.heading + 3) % 4, Straight)
          case Straight => (c.heading, Right)
          case Right => ((c.heading + 1) % 4, Left)
        }
        // right if verical
        case '/' if c.heading % 2 == 0 =>((c.heading + 1) % 4, c.nextTurn)
        case '/' if c.heading % 2 == 1 =>((c.heading + 3) % 4, c.nextTurn)
        case '\\' if c.heading % 2 == 0 =>((c.heading + 3) % 4, c.nextTurn)
        case '\\' if c.heading % 2 == 1 =>((c.heading + 1) % 4, c.nextTurn)
        case _ => (c.heading, c.nextTurn)
      }

      if (map(ny)(nx) == '+') {
        c.nextTurn match {
          case Left => ((c.heading + 3) % 4, Straight)
          case Straight => (c.heading, Right)
          case Right => ((c.heading + 1) % 4, Left)
        }
      }
      else if (map(ny)(nx) == '/')
        ((c.heading + 3) % 4, Straight)
      else if (map(ny)(nx) == '\\')
        ((c.heading + 1) % 4, c.nextTurn)
      else
        (c.heading, c.nextTurn)

      Cart(nx, ny, nh, nnt)
    }

    var crashes = List[Crash]()
    var newCarts = List[Cart]()
    sortedCarts.foreach { case cart =>
      if (newCarts.exists {case Cart(cx, cy, _, _) => cart.x == cx && cart.y == cy}) {
        crashes = Crash(cart.x, cart.y) :: crashes
        newCarts = cart :: newCarts
      }
      else {
        val moved = np(cart)
        if (newCarts.exists {case Cart(cx, cy, _, _) => moved.x == cx && moved.y == cy})
          crashes = Crash(moved.x, moved.y) :: crashes
        newCarts = moved :: newCarts
      }
    }
    (newCarts, crashes)
  }

  def render(carts: List[Cart], map: List[String]): Unit = {
    map.zipWithIndex.foreach {
      case (line, y) => {
        line.zipWithIndex.foreach {case (c, x) =>
          print(carts.find(cart => cart.x == x && cart.y == y).map(_.charactes).getOrElse(c))  }
        println
      }
    }
  }

  var crts = cartsStart

  def stoppingCondition(carts: List[Cart], crashes: List[Crash]): Boolean = {
    carts.length <= 1
  }

  var t = 1
  val result2 = Stream.iterate(crts -> List[Crash]()) {case (carts, crashes) =>
    t = t + 1
    val (nc, ncrs) = tick(carts, linesClean)

    ncrs.foreach { case Crash(crx, cry) =>
      val n = nc.count { case Cart(cx, cy, _, _) => cx == crx && cy == cry}
    }

    val fltr = nc.filter{ case Cart(cax, cay, _, _) =>
      !ncrs.exists{case Crash(cx, cy) => cx == cax && cy == cay}}
    (fltr, ncrs)
  }.find { case (a, b) => stoppingCondition(a, b)}

  println(s"${result2.get._1.head}")
}
