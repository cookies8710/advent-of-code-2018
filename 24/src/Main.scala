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
    val boostedDamage = if (id.side == ImmuneSystem) attackDamage + 36 else attackDamage // immune system wins with 5252 units left
    def effectivePower: Int = numberOfUnits * boostedDamage
    def attackPotential(defender: Group): Int = {
      val multiplier = if (defender.immunities.contains(attackDamageType)) 0
      else if (defender.weaknesses.contains(attackDamageType)) 2
      else 1
      multiplier * effectivePower
    }
    def afterAttackby(attacker: Group): Option[Group] = {
      val casualities = attacker.attackPotential(this) / hitPointsPerUnit
      val newNumberOfUnits = numberOfUnits - casualities
      if (newNumberOfUnits > 0)
        Some(Group(id, newNumberOfUnits, hitPointsPerUnit, immunities, weaknesses, attackDamage, attackDamageType, initiative))
      else
        None
    }
  }

  object Group
  {
    val WithModifiersPattern = "(\\d+) units each with (\\d+) hit points (\\([^\\)]+\\)) with an attack that does (\\d+) ([a-z]+) damage at initiative (\\d+)".r
    val NoModifiersPattern = "(\\d+) units each with (\\d+) hit points with an attack that does (\\d+) ([a-z]+) damage at initiative (\\d+)".r

    val weakTo = "weak to"
    val immuneTo = "immune to"

    def parse(line: String, id: GroupId): Group = {
      line match {
        case NoModifiersPattern(numberOfUnits, hitPointsPerUnit, attackDamage, attackDamageType, initiative) =>
          Group(id, numberOfUnits.toInt,hitPointsPerUnit.toInt, Set.empty, Set.empty, attackDamage.toInt, DamageType.parse(attackDamageType), initiative.toInt)
        case WithModifiersPattern(numberOfUnits, hitPointsPerUnit, damageModifiers, attackDamage, attackDamageType, initiative) =>
          var immunities: Set[DamageType] = Set()
          var weakenesses: Set[DamageType] = Set()
          for (damageModifiers <- damageModifiers.split(";")) {
            val trimmed = damageModifiers.trim.filter(c => c != '(' && c != ')')
            val isImmunity = trimmed.startsWith(immuneTo)
            val sanitized: String = if (trimmed.startsWith(weakTo)) trimmed.drop(weakTo.size) else if (trimmed.startsWith(immuneTo)) trimmed.drop(immuneTo.size) else ""
            val damageTypes = sanitized.split(",").map(_.trim).toList
            if (isImmunity) immunities = damageTypes.map(DamageType.parse).toSet else weakenesses = damageTypes.map(DamageType.parse).toSet
          }
          Group(id, numberOfUnits.toInt,hitPointsPerUnit.toInt,immunities, weakenesses, attackDamage.toInt,DamageType.parse(attackDamageType), initiative.toInt)
      }
    }
  }

  trait Side
  {
    def other: Side = ???

  }
  object NoSide extends Side
  object ImmuneSystem extends Side
  {
    override def other: Side = Infection
    override def toString: String = "Immune System"
  }
  object Infection extends Side
  {
    override def other: Side = ImmuneSystem
    override def toString: String = "Infection"
  }
  var side: Side = NoSide
  var counter: Int = 0
  val input = realInput
  val initialGroups: Map[GroupId, Group] = input.flatMap {
    case "Immune System:" =>
      side = ImmuneSystem
      counter = 0
      None
    case "Infection:" =>
      side = Infection
      counter = 0
      None
    case s if s.trim.isEmpty =>
      None
    case grp =>
      counter = counter + 1
      val groupId = GroupId(side, counter)
      Some(groupId -> Group.parse(grp, groupId))
  }.toMap

  class State(val initialGroups: Map[GroupId, Group])
  {
    val winningArmy: Option[Side] = initialGroups.map(_._1.side).toList.distinct match {
      case List(winner) => Some(winner)
      case _ => None
    }
    val fighting: Boolean = initialGroups.map(_._1.side).toSet.size > 1
    def fight(): State = {
      // phase I : target selection
      println("Phase I - target selection")
      var targets: List[(GroupId, Option[GroupId])] = List()
      val targetSelectionOrder = initialGroups.values.toList.sortWith((a, b) => a.effectivePower > b.effectivePower || (a.effectivePower == b.effectivePower &&
        a.initiative >= b.initiative))
      println(s"Target selection order:")
      targetSelectionOrder.foreach(println)
      for(attackingGroup <- targetSelectionOrder)
      {
        val defendingGroup: Option[Group] = initialGroups.values.filter(pd => pd.id.side == attackingGroup.id.side.other)
          .filterNot(pd => targets.flatMap(_._2).contains(pd.id)) // not defending yet
          .filter(df => attackingGroup.attackPotential(df) > 0)
          .toList
          .sortWith((a, b) => {
            attackingGroup.attackPotential(a) > attackingGroup.attackPotential(b) ||
              ( attackingGroup.attackPotential(a) == attackingGroup.attackPotential(b) &&
                (a.effectivePower > b.effectivePower || ( a.effectivePower == b.effectivePower && a.initiative >= b.initiative)))
          }).headOption
        targets = (attackingGroup.id -> defendingGroup.map(_.id)) :: targets
      }

      // phase II: attacking
      println("Phase II - attacking")
      var newState: Map[GroupId, Group] = Map()
      val attackOrder = initialGroups.values.toList.sortBy(_.initiative).map(_.id).reverse
      var dead: List[GroupId] = List()
      for(attackingGroupId <- attackOrder)
      {
        val attackingGroup: Group = newState.getOrElse(attackingGroupId, initialGroups(attackingGroupId))
        val defender = targets.find(_._1 == attackingGroup.id).map(_._2).get
        defender match {
          case Some(defendingGroupId) =>
            if (dead.contains(attackingGroupId)) {
              newState = newState.updated(defendingGroupId, initialGroups(defendingGroupId))
            }
            else {
              println(s"${attackingGroup.id} attacks $defendingGroupId")
              val aa = initialGroups(defendingGroupId).afterAttackby(attackingGroup)
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
      val carry = initialGroups.filterKeys(!targeted.contains(_))
      newState = newState ++ carry
      new State(newState)
    }

    def render() = {
      val groupsBySide = initialGroups.values.groupBy(_.id.side)
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
  val result = state.initialGroups.values.map(_.numberOfUnits).sum
  println(s"Winning army (${state.winningArmy}) has $result units")
}