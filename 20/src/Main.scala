import scala.collection.mutable

object Main extends App {
  val realInput = scala.io.Source.fromFile("input").getLines().toStream.head
  case class TestInput(regex: String, expectedResult: Int)
  val testInputs = List(
    TestInput("^WNE$", 3),
    TestInput("^ENWWW(NEEE|SSE(EE|N))$", 10),
    TestInput("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$", 18),
    TestInput("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$", 23),
    TestInput("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$", 31))

  object Node
  {
    private def findGroupDelimiters(s: String): List[Int] = {
      var delimiters = List[Int]()
      var level = 0
      for (i <- 0 until s.length)
      {
        s(i) match {
          case '|' if level == 0 => delimiters = i :: delimiters
          case '(' => level = level + 1
          case ')' => level = level - 1
          case _ =>
        }
      }

      delimiters.reverse
    }

    private def findSequentialBoundaries(string: String): List[Int]= {
      var boundaries = List[Int]()
      var level = 0
      for (i <- 0 until string.length)
      {
        string(i) match {
          case '(' =>
            if (level == 0) boundaries = i :: boundaries
            level = level + 1
          case ')' =>
            level = level - 1
            if (level == 0) boundaries = i :: boundaries
          case _ =>
        }
      }

      boundaries.reverse
    }

    private def childrenStrs(string: String, indices: List[Int]): List[String] =
    {
      var l = 0
      var substrings = List[String]()
      for (i <- indices)
      {
        substrings = string.substring(l, i) :: substrings
        l = i + 1
      }

      substrings = string.substring(l, string.length) :: substrings
      substrings.reverse
    }

    def parse(string: String): Node = {
      val input = if (string.length > 1 && string.head == '(' && string.last == ')')
        string.substring(1, string.length - 1) else string // unwrap

      val groupDelimiters = findGroupDelimiters(input)

      if (groupDelimiters.nonEmpty)
        new Grouped(childrenStrs(string, groupDelimiters).map(parse))
      else
      {
        val sequentialBoundaries = findSequentialBoundaries(string)
        if (sequentialBoundaries.nonEmpty)
          new Sequential(childrenStrs(string, sequentialBoundaries).map(parse))
        else
          new Word(new String(string.filter(c=>c!='$'&&c!='^').toCharArray))

      }
    }
  }

  abstract class Node
  {
    def depth: Int
    def count(predicate: Node => Boolean): Int
    def generateWords(prefix: String): Stream[String]
    def countPossibleWords: Long
    def longestWord: String
    def shorten(string: String): String = {
      var end = false
      var input = string
      while (!end) {
        var currentPosition = 0 -> 0
        val visited: Map[(Int, Int), Int] = Map(currentPosition -> 0)
        end = true
        for (i <- 0 until input.length if end)
        {
          currentPosition = input(i) match {
            case 'S' => (currentPosition._1, currentPosition._2 + 1)
            case 'N' => (currentPosition._1, currentPosition._2 - 1)
            case 'E' => (currentPosition._1 + 1, currentPosition._2)
            case 'W' => (currentPosition._1 - 1, currentPosition._2)
            case _ => currentPosition
          }

          if (visited.contains(currentPosition))
          {
            end = false
            input = input.substring(0, visited(currentPosition)) + input.substring(i+1)
          }

          visited.updated(currentPosition, i)
        }
      }
      input
    }
    def explore(state: State, positions: List[(Int, Int)]): List[(Int, Int)]
  }

  class Sequential(val children: List[Node]) extends Node
  {
    override def count(predcate: Node => Boolean): Int = (if (predcate(this)) 1 else 0) + children.map(_.count(predcate)).sum
    override def depth: Int = 1 + children.map(_.depth).max
    override def countPossibleWords: Long = children.map(_.countPossibleWords).product
    override def generateWords(prefix: String): Stream[String] = {
      var words = Stream(prefix)
      for (child <- children)
          words = words.flatMap(child.generateWords)
      words
    }

    override def longestWord: String = {
      var longest = ""
      for (childsLongest <- children.map(_.longestWord))
        longest = longest + childsLongest
      super.shorten(longest)
    }

    override def explore(state: State, p: List[(Int, Int)]): List[(Int, Int)] = {
      var positions = p
      for (child <- children)
          positions = child.explore(state, positions).distinct
      positions
    }
  }

  class Grouped(val children: List[Node]) extends Node
  {
    override def count(f: Node => Boolean): Int = (if (f(this)) 1 else 0) + children.map(_.count(f)).sum
    override def depth: Int = 1 + children.map(_.depth).max
    override def countPossibleWords: Long = children.map(_.countPossibleWords).sum
    override def longestWord: String = children.map(_.longestWord).maxBy(_.length)
    override def generateWords(prefix: String): Stream[String] = children.toStream.flatMap(ch => ch.generateWords(prefix))
    override def explore(state: State, p: List[(Int, Int)]): List[(Int, Int)] = children.flatMap(_.explore(state, p)).distinct
  }

  class Word(val w: String) extends Node
  {
    override def count(f: Node => Boolean): Int = if (f(this)) 1 else 0
    override def countPossibleWords: Long = 1
    override def longestWord: String = super.shorten(w)
    override def depth: Int = 1
    override def generateWords(prefix: String): Stream[String] = Stream(prefix + w)
    override def explore(state: State, p: List[(Int, Int)]): List[(Int, Int)] = p.map(state.explore(_, w.toList))
  }

  val inputs = testInputs
  testInputs.foreach {ti =>
    val root = Node.parse(ti.regex)
    val result = root.longestWord

    if (result.length == ti.expectedResult)
      println(s"passed $ti")
    else {
      println(s"INCORRECT $ti:")
      println(s"Compute has length ${result.length}: $result ")
      println(s"Expected: ${ti.expectedResult}")
      println()
    }
  }

  println("\nParsing input")
  val root = Node.parse(realInput)
  println(s"\tdepth: ${root.depth}\n\t# of nodes: ${root.count(_=> true)}\n\t# of group nodes:${root.count(_.isInstanceOf[Grouped])}")

  val longest = root.longestWord
  println(s"Longest word has length ${longest.length}: $longest")
  println(s"# of words ${root.countPossibleWords}")

  val state = new State
  root.explore(state, List(0->0))
  state.conserve()
  state.render(1)

  // part 2
  state.flood(999)
  state.render(1)
  val result2 = state.count(Open)
  println(s"Part 2 result: $result2")

  trait Tile
  object Open extends Tile
  object Door extends Tile
  object Wall extends Tile
  object DoorOrWall  extends Tile
  object Start extends Tile
  object Flood extends Tile

  def even(i: Int): Boolean = (i & 1) == 0
  def odd(i: Int): Boolean = !even(i)

  def tileToChar(tile: Tile, position: (Int, Int)): Char = tile match {
    case Wall => '#'
    case Open=> '.'
    case DoorOrWall => '?'
    case Door if odd(position._1) => '|'
    case Door if even(position._1) => '-'
    case Start => 'X'
    case Flood => '~'
  }

  class State {
    def openDefault(i :(Int, Int)): Tile = i match {
      case (x, y) if odd(x) && odd(y) => Wall
      case (x, y) if even(x) && even(y) => Open
      case _ => DoorOrWall
    }
    def closedDefault(i :(Int, Int)): Tile = i match {
      case (x, y) if odd(x) && odd(y) => Wall
      case (x, y) if even(x) && even(y) => Open
      case _ => Wall
    }

    var map: mutable.Map[(Int, Int), Tile] = mutable.Map().withDefault(openDefault)
    map.update(0->0, Start)

    def flood(doors: Int): Unit = {
      map.update(0->0, Flood)
      for(j <- 0 until doors)
      {
        def floodDir(c: (Int, Int), d: (Int, Int)): Unit = {
          val doorPosition = (c._1 + d._1, c._2 + d._2)
          val roomPosition = (c._1 + 2*d._1, c._2 + 2*d._2)

          if (map(doorPosition)==Door && map(roomPosition)==Open)
            map.update(roomPosition, Flood)
        }

        def flood1(c: (Int, Int)): Unit = {
          floodDir(c, 0->1)
          floodDir(c, 0->(-1))
          floodDir(c, 1->0)
          floodDir(c, (-1)->0)
        }

        val underWater = map.filter(_._2 == Flood).toList
        underWater.map(_._1).foreach(flood1)
      }
    }

    def count(t: Tile): Int = {
      val xa = map.keys.map(_._1).min
      val xb = map.keys.map(_._1).max
      val ya = map.keys.map(_._2).min
      val yb = map.keys.map(_._2).max

      var c = 0
      for (y <- ya to yb; x <- xa to xb)
      {
        if (map(x->y) == t)
          c = c + 1
      }
      c
    }

    def render(e: Int): Unit = {
      //val e = if (ext) 2 else 0
      val xa = map.keys.map(_._1).min - e
      val xb = map.keys.map(_._1).max + e
      val ya = map.keys.map(_._2).min - e
      val yb = map.keys.map(_._2).max + e

      for (y <- ya to yb; x <- xa to xb)
      {
        if (x==xa)
          println()
        print(tileToChar(map(x->y), x->y))
      }
      println()
    }

    def step(position: (Int, Int), direction: Char): (Int, Int) = direction match {
        case 'S' =>
          map.update(position._1 -> (position._2 + 1), Door)
          (position._1, position._2 + 2)
        case 'N' =>
          map.update(position._1 -> (position._2 - 1), Door)
          (position._1, position._2 - 2)
        case 'E' =>
          map.update((position._1 + 1) -> position._2, Door)
          (position._1 + 2, position._2)
        case 'W' =>
          map.update((position._1 - 1) -> position._2, Door)
          (position._1 - 2, position._2)
        case c =>
          println(s"step $c omitted")
          position
      }

    def explore(startPosition: (Int, Int), steps: List[Char]): (Int, Int) =
      steps match {
        case direction :: rest =>
          val newPosition = step(startPosition, direction)
          explore(newPosition, rest)
        case _ =>
          startPosition
      }

    def conserve(): Unit = {
      val xa = map.keys.map(_._1).min
      val xb = map.keys.map(_._1).max
      val ya = map.keys.map(_._2).min
      val yb = map.keys.map(_._2).max
      for (y <- ya to yb; x <- xa to xb)
        if (map(x->y) == DoorOrWall)
          map.update(x->y, Wall)

      map = map.withDefault(closedDefault)
    }
  }
}
