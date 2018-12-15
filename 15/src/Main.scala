object Main extends App {
  type Coordinates = (Int, Int)
  object ShortestPathOrdering extends Ordering[List[Coordinates]] {
    override def compare(xx: List[(Int, Int)], yy: List[Coordinates]): Int = {
      val x = xx.reverse
      val y = yy.reverse
      if (x.length != y.length)
        x.length - y.length
      else if (x.isEmpty)
        0
      else if (x.head._2 != y.head._2)
        x.head._2 - y.head._2
      else
        x.head._1 - y.head._1
    }
  }

  case class Entity(var x: Int, var y: Int, isElf: Boolean, var hp: Int) {
    def move(nx: Int, ny: Int): Unit = {
      x = nx
      y = ny
    }

    def damage(i: Int): Boolean = {
      hp = hp - i; isDead
    }

    //val attackPower: Int = if (isElf) 14 else 3 // part 2
    val attackPower: Int = 3
    def copy: Entity = Entity(x, y, isElf, hp)
    def isDead: Boolean = hp <= 0
    def isAlive: Boolean = !isDead
    def dist(other: Entity): Int = Math.abs(other.x - x) + Math.abs(other.y - y)
    def getChar: Char = if (isElf) 'E' else 'G'
    def getStat: String = s"$getChar($hp) [$x, $y]"
    def getPosition: Coordinates = x -> y
  }

  object ReadOrdering extends Ordering[Coordinates]
  {
    override def compare(x: Coordinates, y: Coordinates): Int =
      if (x._2 != y._2) x._2- y._2 else x._1 - y._1
  }

  object Entity
  {
    implicit object EntityOrdering extends Ordering[Entity]
    {
      override def compare(x: Entity, y: Entity): Int = ReadOrdering.compare(x.getPosition, y.getPosition)
    }
  }

  case class State(map: Map[Coordinates, Boolean], entities: List[Entity], elfDied: Boolean)
  {
    def combatEnd: Boolean = entities.filter(!_.isDead).map(_.isElf).distinct.length != 2
    def computeOutcome(round: Int): Int = round * hpSum
    def hpSum: Int = entities.filter(!_.isDead).map(_.hp).sum

    def flood(discovered: Map[Coordinates, List[Coordinates]], obstacles: Set[Coordinates]):Map[Coordinates, List[Coordinates]] =
    {
      val discoveredPositions = discovered.keys.toSet
      val newlyFlooded: Set[(Coordinates, List[Coordinates])] =
        discoveredPositions.flatMap{case (x,y) =>
          List((x,y-1), (x-1,y), (x+1,y), (x,y+1))
            .filter(!discoveredPositions.contains(_)) // must not be already discovered
            .filter(!obstacles.contains(_)) // most not contain obstacle
            .map(p => p -> (p::discovered(x->y)))}

      // newlyFlooded may have reached the same position by different routes - then chose the one with "earlier" beginning
      val newlyFloodedResolved: Map[Coordinates, List[Coordinates]] = newlyFlooded.groupBy(_._1).values
        .map(group => {
          val newPos = group.head._1
          val paths = group.map(_._2)

          val shortestPart = paths.toList.sorted(ShortestPathOrdering).head
          newPos -> shortestPart
        })
        .toMap
      discovered ++ newlyFloodedResolved
    }

    def findShortestPath(source: Coordinates, target: Coordinates, entities: List[Entity]): Option[List[Coordinates]] = {
      val walls = map.filter(kv => !kv._2).keys.toSet
      val liveUnits = entities.filter(!_.isDead).map(_.getPosition).toSet
      val obstacles = walls ++ liveUnits
      var reachables = Map(source -> List[Coordinates]())

      var finished = false
      while (!finished) {
        val newReachables = flood(reachables, obstacles)
        finished = newReachables.size == reachables.size || newReachables.contains(target)
        reachables = newReachables
      }
      reachables.get(target).map(_.reverse)
    }

    def openSquares(map: Map[Coordinates, Boolean], entities: List[Entity])(target: Entity): List[Coordinates] =
    {
      List((target.x - 1, target.y), (target.x + 1, target.y),
        (target.x, target.y + 1), (target.x, target.y - 1))
        .filter(map) // must not be wall
        .filter { case (x, y) => !entities.exists // there's no live entity on that square
      {case ent@Entity(ex, ey, _, _) if !ent.isDead => ex == x && ey == y case _ => false } }
    }

    def inRange(entity: Entity, targets: List[Entity]): List[Entity] = targets.filter(_.dist(entity) == 1)

    def attack(power: Int, targets: List[Entity]): Boolean = {
      if (targets.nonEmpty)
        targets.minBy(_.hp).damage(power)
      targets.nonEmpty
    }

    def moveShortest(entity: Main.Entity, opens: List[Coordinates], units: List[Entity]): Boolean =
    {
      if (opens.isEmpty)
        return false

      val closests = opens
        .map(open => open -> findShortestPath(entity.getPosition, open, units)) // find shortest path to each open square
        .filter(_._2.isDefined)// get those which are reachable
        .map(kv => kv._1 -> kv._2.get)
        .sortBy(_._2.size)

      if (closests.isEmpty)
        return false

      val smallest = closests.head._2.size // fewest num of steps
      val shortests = closests // paths with fewest num of steps
        .filter(_._2.size == smallest)
        .sortWith((a,b) => ReadOrdering.compare(a._1, b._1) <= 0) // paths ordered by target's reading order
      val winner = shortests
        .head
      val path=winner._2
      entity.move(path.head._1, path.head._2)
      true
    }

    def getEnemyTargets(current: Entity, entities: List[Entity]): List[Entity] =
      entities.filter(!_.isDead).filter(_.isElf == !current.isElf).sorted
    def tryAttack(current: Entity, targets: List[Entity]): Boolean = attack(current.attackPower, inRange(current, targets))

    def turn: State = {
      // 1. order entities
      val nextStateEntities = entities.sorted.map(_.copy)

      // 2. sequentially act on entities - for each entity X:
      nextStateEntities.foreach(current => {
        if (current.isAlive) { // skip Dead
          // 3. X identifies targets, if no targets, it ends turn
          val targets = getEnemyTargets(current, nextStateEntities)
          // Try attack if a target is in range, otherwise move and try then
          if (targets.nonEmpty && !tryAttack(current, targets)) {
            // 4. X identifies open squares in range of each target
            val openSqs = targets.flatMap(openSquares(map, nextStateEntities))
            // 5. try to move to the closest in range
            if (moveShortest(current, openSqs, nextStateEntities)) {
              // 6. after moving or if A was in range already, unit attacks the
              // enemy in range with lowest HP; attack power 3
              // if enemy dies, empty space appears
              tryAttack(current, targets)
            }
          }
        }
      })
      // Move the alive entities to next state
      val (alive, dead) = nextStateEntities.partition(_.isAlive)
      State(map, alive, elfDied || dead.exists(_.isElf))
    }

    def render = {
      val xs = map.keys.map(_._1)
      val ys = map.keys.map(_._2)

      val (l, r) = (xs.min, xs.max)
      val (u, d) = (ys.min, ys.max)

      for (y <- u to d; x <- l to r) {
        if (x == l) println()
        val mc = if (!map(x -> y)) '#' else '.'
        val en = entities.find
        {case Entity(ex, ey, ie, _) => ex == x && ey == y}.map(_.getChar)

        print(en.getOrElse(mc))
      }
      println("\nHPs:")
      entities.foreach(e => println(e.getStat))
      println(if (elfDied) "ELF DIED" else "no elf dead yet")
    }
  }

  object State
  {
    def parse(i: List[String]): State = {
      val in = i.zipWithIndex.flatMap {case (line, y) =>
        line.zipWithIndex.map {case (char, x) => ((x, y), char)}}.toMap

      val goblins = in.filter { case (_, char) => char == 'G'}.keys
        .map {case (x, y) => Entity(x, y, false, 200)}.toList
      val elves = in.filter { case (_, char) => char == 'E'}.keys
        .map {case (x, y) => Entity(x, y, true, 200)}.toList

      // sematics: accessibility => true is acc, false is not (wall)
      val walls = in.filter { case (_, char) => char == '#'}.keys
        .map {case (x, y) => (x, y) -> false}.toMap.withDefaultValue(true)

      State(walls, elves ++ goblins, false)
    }
  }

  val lines = scala.io.Source.fromFile("input").getLines().toList
  var state = State.parse(lines)
  var combatEnd = false
  var i = 0
  while (!combatEnd) {
    println(s"\n$i")
    state.render
    state = state.turn
    combatEnd = state.combatEnd
    i = i + 1
  }

  val turns = i - 1
  state.render
  println(s"Combat ended, i = $turns, hpsum: ${state.hpSum}, outcome: ${state.computeOutcome(turns)}")
}
