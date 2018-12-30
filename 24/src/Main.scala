object Main extends App {
  val realInput = scala.io.Source.fromFile("input").getLines.toList
  val testInput = scala.io.Source.fromFile("test-input").getLines.toList

  trait DamageType
  object Fire extends DamageType
  object Cold extends DamageType
  object Slashing extends DamageType
  object Bludgeoning extends DamageType
  object Radiation extends DamageType
  object DamageType
  {
    def parse(s: String): DamageType = s match {
      case "fire" => Fire
      case "cold" => Cold
      case "slashing" => Slashing
      case "bludgeoning" => Bludgeoning
      case "radiation" => Radiation
      case other => throw new RuntimeException(s"Unknown damage type '$other'")
    }
  }

  case class GroupId(side: Side, n: Int)
  {
    override def toString: String = s"$side group $n"
  }
  case class Group(id: GroupId, numberOfUnits: Int, hitPointsPerUnit: Int, immunities: Set[DamageType], weaknesses: Set[DamageType], attackDamage: Int, attackDamageType: DamageType, initiative: Int)
  {
    override def toString: String = s"Group $id with $numberOfUnits units"
    val boostedDamage = if (id.side == Imm) attackDamage + 36 else attackDamage // immune system wins with 5252 units left
    def effectivePower: Int = numberOfUnits * boostedDamage
    def attackPotential(defender: Group): Int = {
      val mul = if (defender.immunities.contains(attackDamageType)) 0 else if (defender.weaknesses.contains(attackDamageType)) 2 else 1
      mul * effectivePower
    }
    def afterAttackby(attacker: Group): Option[Group] = {
      val casualities = attacker.attackPotential(this) / hitPointsPerUnit
      val nu = numberOfUnits - casualities
      if (nu > 0)
        Some(Group(id, nu, hitPointsPerUnit, immunities, weaknesses, attackDamage, attackDamageType, initiative))
      else
        None
    }
  }

  object Group
  {
    val NoModifiersPattern = "(\\d+) units each with (\\d+) hit points (\\([^\\)]+\\)) with an attack that does (\\d+) ([a-z]+) damage at initiative (\\d+)".r
    val WithModifiersPattern = "(\\d+) units each with (\\d+) hit points with an attack that does (\\d+) ([a-z]+) damage at initiative (\\d+)".r

    val wt = "weak to"
    val it = "immune to"

    def parse(line: String, id: GroupId): Group = {
      line match {
        case WithModifiersPattern(numberOfUnits, hitPointsPerUnit, attackDamage, attackDamageType, initiative) =>
          Group(id, numberOfUnits.toInt,hitPointsPerUnit.toInt, Set.empty, Set.empty, attackDamage.toInt, DamageType.parse(attackDamageType), initiative.toInt)
        case NoModifiersPattern(nu, hps, imw, attdmg, atttype, init) =>
          var iset: Set[DamageType] = Set()
          var wset: Set[DamageType] = Set()
          for (iwg <- imw.split(";")) {
            val koo = iwg.trim.filter(c => c != '(' && c != ')')
            val imm = koo.startsWith(it)
            val sanitized: String = if (koo.startsWith(wt)) koo.drop(wt.size) else if (koo.startsWith(it)) koo.drop(it.size) else ""
            val tps = sanitized.split(",").map(_.trim).toList
            if (imm) iset = tps.map(DamageType.parse).toSet else wset = tps.map(DamageType.parse).toSet
          }
          Group(id, nu.toInt,hps.toInt,iset, wset, attdmg.toInt,DamageType.parse(atttype), init.toInt)
      }
    }
  }

  trait Side
  {
    def other: Side = ???

  }
  object Noside extends Side
  object Imm extends Side
  {
    override def other: Side = Inf
    override def toString: String = "Immune System"
  }
  object Inf extends Side
  {
    override def other: Side = Imm

    override def toString: String = "Infection"
  }
  var side: Side = Noside
  var counter: Int = 0
  val input = realInput
  val initialGroups: Map[GroupId, Group] = input.flatMap {
    case "Immune System:" =>
      side = Imm
      counter = 0
      None
    case "Infection:" =>
      side = Inf
      counter = 0
      None
    case s if s.trim.isEmpty =>
      None
    case grp =>
      counter = counter + 1
      val groupId = GroupId(side, counter)
      Some(groupId -> Group.parse(grp, groupId))
  }.toMap

  class State(val igrs: Map[GroupId, Group])
  {
    val winningArmy: Option[Side] = igrs.map(_._1.side).toList.distinct match {
      case List(winner) => Some(winner)
      case _ => None
    }
    val fighting: Boolean = igrs.map(_._1.side).toSet.size > 1
    def fight(): State = {
      // phase I : target selection
      println("Phase I - target selection")
      var targets: List[(GroupId, Option[GroupId])] = List()
      val targetSelectionOrder = igrs.values.toList.sortWith((a, b) => a.effectivePower > b.effectivePower || (a.effectivePower == b.effectivePower &&
        a.initiative >= b.initiative))
      println(s"Target selection order:")
      targetSelectionOrder.foreach(println)
      for(attackingGroup <- targetSelectionOrder)
      {
        val defendingGroup: Option[Group] = igrs.values.filter(pd => pd.id.side == attackingGroup.id.side.other)
          .filterNot(pd => targets.flatMap(_._2).contains(pd.id)) // not defending yer
          .filter(df => attackingGroup.attackPotential(df) > 0)
          .toList
          .sortWith((a, b) => {
            attackingGroup.attackPotential(a) > attackingGroup.attackPotential(b) ||
              ( attackingGroup.attackPotential(a) == attackingGroup.attackPotential(b) &&
                (a.effectivePower > b.effectivePower || ( a.effectivePower == b.effectivePower && a.initiative >= b.initiative)))
          }).headOption
        targets = (attackingGroup.id -> defendingGroup.map(_.id)) :: targets
//        println(s"Attacker ${attackingGroup.id} chose target: $defendingGroup")
      }

      // phase II: attacking
      println("Phase II - attacking")
      var newState: Map[GroupId, Group] = Map()
      val attackOrder = igrs.values.toList.sortBy(_.initiative).map(_.id).reverse
      var dead: List[GroupId] = List()
      for(attackingGroupId <- attackOrder)
      {
        val attackingGroup: Group = newState.getOrElse(attackingGroupId, igrs(attackingGroupId))
        val defender = targets.find(_._1 == attackingGroup.id).map(_._2).get
        defender match {
          case Some(defendingGroupId) =>
            if (dead.contains(attackingGroupId)) {
              newState = newState.updated(defendingGroupId, igrs(defendingGroupId))
            }
            else {
              println(s"${attackingGroup.id} attacks $defendingGroupId")
              val aa = igrs(defendingGroupId).afterAttackby(attackingGroup)
              newState = aa match {
                case Some(defendingGroupAfterDefense) => newState.updated(defendingGroupId, defendingGroupAfterDefense)
                case _ =>
                  dead = defendingGroupId :: dead
                  newState
              }
            }
          case _ =>
        }
      }

      val targeted = targets.flatMap(_._2).toSet
      val carry = igrs.filterKeys(!targeted.contains(_))
      newState = newState ++ carry
      new State(newState)
    }

    def render() = {
      val groupsBySide = igrs.values.groupBy(_.id.side)
      for ((side, groups) <- groupsBySide)
        {
          println(s"$side:")
          groups.foreach(g => println(s"${g.id} contains ${g.numberOfUnits} units"))
          println()
        }

    }
  }

  var state = new State(initialGroups)
  state.render()
  var round = 1
  while (state.fighting)
  {
    println(s"Round $round")
    println("Start:")
    state.render()
    state = state.fight()
    println("End:")
    state.render()
    round = round + 1
  }
  val result = state.igrs.values.map(_.numberOfUnits).sum
  println(s"Winning army (${state.winningArmy}) has $result units")
}