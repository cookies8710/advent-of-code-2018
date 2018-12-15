object Main extends App {
  val lines = scala.io.Source.fromFile("input").getLines.toList
  val testLines = List(
    "initial state: #..#.#..##......###...###",
    "",
    "...## => #",
    "..#.. => #",
    ".#... => #",
    ".#.#. => #",
    ".#.## => #",
    ".##.. => #",
    ".#### => #",
    "#.#.# => #",
    "#.### => #",
    "##.#. => #",
    "##.## => #",
    "###.. => #",
    "###.# => #",
    "####. => #")

  case class Rule(mask: List[Boolean], result: Boolean)
  object Rule
  {
    val pattern = "([\\.#]+)\\s+=>\\s+([\\.#])".r
    def parse(s: String): Rule = s match {
      case pattern(mask, result) =>
        Rule(mask.map(_ == '#').toList, result == "#")
    }
  }

  case class State(state: List[Boolean], startIndex: Int)
  {
    def step(rules: Set[Rule]): State = {
      def getSlice(i: Int): List[Boolean] = {
        def potFull(j: Int): Boolean = j >= 0 && j < state.length && state(j)
        (i - 2 to i + 2).map(potFull).toList
      }

      def applyRule(rules: Set[Rule])(slice: List[Boolean]): Boolean =
        rules.find(_.mask == slice).exists(_.result)

      // spreading to left and right
      val leftadd = (-5 until 0)
        .map(getSlice)
        .map(applyRule(rules)).toList
      val rightadd = (state.length until state.length + 5)
        .map(getSlice)
        .map(applyRule(rules)).reverse.dropWhile(!_).reverse.toList
      // new state from the previous
      val newState = state.indices
        .map(getSlice)
        .map(applyRule(rules)).toList

      // if the plants spread left, prepend and decrease start index
      if (leftadd.exists(x => x))
      {
        val newAtLeft = leftadd.dropWhile(!_)
        val newStartIndex = startIndex - newAtLeft.length
        State(newAtLeft ++ newState ++ rightadd, newStartIndex)
      }
      // otherwise try drop empty pots from left and increase start index
      else
      {
        val (empty, nonEmpty) = newState.span(!_)
        val newStartIndex = startIndex + empty.length
        State(nonEmpty ++ rightadd, newStartIndex)
      }
    }

    def sumPots: Int = Stream.iterate(startIndex)(_ + 1)
      .zip(state)
      .filter(_._2).map(_._1).sum

    def render: Unit = {
      println(s"Starting index: $startIndex")
      state.foreach(p => print(if (p) '#' else '.'))
      println()
    }
  }

  object State
  {
    val reg = "initial state: ([\\.#]+)".r
    def parse(s: String, si: Int = 0): State =
      s match {
        case reg(st) => State(st.map(_ == '#').toList, si)
      }
  }

  val input = lines//testLines

  val state = State.parse(input.head)
  val rules = input.drop(2).map(Rule.parse).toSet

  val finalGen = (1 to 20).foldLeft(state){case (oldState, _) => oldState.step(rules)}
  finalGen.render
  val result1 = finalGen.sumPots
  println(s"result 1 : $result1")

  //Stream.iterate(1)(_ + 1).foldLeft(1 -> state){case ((oldgen, oldState), generation) => oldState.step(rules)}
  /*val kkk = Stream.iterate(1 -> state){case (generation, oldState) => generation + 1 -> oldState.step(rules)}
  val lll = kkk.sliding(2).map { case a #:: b #:: _ => b._2.sumPots - a._2.sumPots -> a._1 }

  val magic = 88

  val firstIndex = Stream.iterate(1)(_ + 1).find(i => lll.map(_._1).slice(i, i + 5).forall(_ == magic)).get
  println(firstIndex)*/



  /*val testState = State.parse("initial state: #...##...#.#..#..#...#....#", 1)
  testState.step(rules).render*/
}
