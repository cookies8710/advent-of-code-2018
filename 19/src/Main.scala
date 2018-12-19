object Main extends App {
  type Registers = Map[Int, Int]

  abstract class Instruction {
    val name: String = getClass.getSimpleName.toLowerCase

    def apply(input: List[Int], registers: Registers): Registers = {
      def tryGetRegister(registers: Registers, i: Int): Option[Int] = if (registers.contains(i)) Some(registers(i)) else None
      val RA = tryGetRegister(registers, input(0))
      val RB = tryGetRegister(registers, input(1))
      registers.updated(input(2), apply(input(0), input(1), RA, RB))
    }
    def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int
  }

  class Addr extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = RA.get + RB.get
  }

  class Addi extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = RA.get + B
  }

  class Mulr extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = RA.get * RB.get
  }

  class Muli extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = RA.get * B
  }

  class Banr extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = RA.get & RB.get
  }

  class Bani extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = RA.get & B
  }

  class Borr extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = RA.get | RB.get
  }

  class Bori extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = RA.get | B
  }

  class Setr extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = RA.get
  }

  class Seti extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = A
  }

  class Gtir extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = if (A > RB.get) 1 else 0
  }

  class Gtrr extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = if (RA.get > RB.get) 1 else 0
  }

  class Gtri extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = if (RA.get > B) 1 else 0
  }

  class Eqir extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = if (A == RB.get) 1 else 0
  }

  class Eqrr extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = if (RA == RB) 1 else 0
  }

  class Eqri extends Instruction {
    override def apply(A: Int, B: Int, RA: Option[Int], RB: Option[Int]): Int = if (RA.get == B) 1 else 0
  }


  object Instruction
  {
    val Instructions = Set(
      new Addr(),
      new Addi(),
      new Mulr(),
      new Muli(),
      new Banr(),
      new Bani(),
      new Borr(),
      new Bori(),
      new Setr(),
      new Seti(),
      new Gtir(),
      new Gtri(),
      new Gtrr(),
      new Eqir(),
      new Eqri(),
      new Eqrr())
    val NameToInstruction = Instructions.map(i => i.name -> i).toMap
  }

  val testInput = List(
    "#ip 0",
    "seti 5 0 1",
    "seti 6 0 2",
    "addi 0 1 0",
    "addr 1 2 3",
    "setr 1 0 0",
    "seti 8 0 4",
    "seti 9 0 5")
  val realInput = scala.io.Source.fromFile("input").getLines.toList
  val input = realInput

  class Processor(input: List[String], reg0: Int = 0)
  {
    val ipRegisterRegex = "#ip (\\d+)".r

    var registers = (1 to 5).map(_ -> 0).toMap.updated(0, reg0)
    val ipRegister = input.head match { case ipRegisterRegex(n) => n.toInt }

    def toPairs(line: String): (Instruction, List[Int]) = {
      val nl = line.split(' ')
      (Instruction.NameToInstruction(nl.head), nl.tail.map(_.toInt).toList)
    }

    val program = input.tail.map(toPairs)
    def statusString: String = s"ip=${ip} [${registers.toList.sortBy(_._1).map(_._2).mkString(", ")}]"

    def ip = registers(ipRegister)

    def fetch: Option[(Instruction, List[Int])] = {
      if (ip < program.length)
        Some(program(ip))
      else None
    }

    def step(): Boolean = {
      fetch.exists { case (instr, params) =>
        registers = instr.apply(params, registers)
        registers = registers.updated(ipRegister, ip + 1)
        true
      case _ => false
      }
    }
  }

  val processor = new Processor(input, 0)
  var tick = 0L

  while (processor.step())
    tick = tick + 1

  println(processor.statusString)
}
