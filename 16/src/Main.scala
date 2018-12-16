object Main extends App {
  type Registers = Map[Int, Int]

  abstract class Instruction {
    def apply(input: List[Int], registers: Registers): Registers =
      registers.updated(input(3), apply(input(1), input(2), registers(input(1)), registers(input(2))))
    def apply(A: Int, B: Int, RA: Int, RB: Int): Int
  }

  class Addr extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = RA + RB
  }

  class Addi extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = RA + B
  }

  class Mulr extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = RA * RB
  }

  class Muli extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = RA * B
  }

  class Banr extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = RA & RB
  }

  class Bani extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = RA & B
  }

  class Borr extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = RA | RB
  }

  class Bori extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = RA | B
  }

  class Setr extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = RA
  }

  class Seti extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = A
  }

  class Gtir extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = if (A > RB) 1 else 0
  }

  class Gtrr extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = if (RA > RB) 1 else 0
  }

  class Gtri extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = if (RA > B) 1 else 0
  }

  class Eqir extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = if (A == RB) 1 else 0
  }

  class Eqrr extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = if (RA == RB) 1 else 0
  }

  class Eqri extends Instruction {
    override def apply(A: Int, B: Int, RA: Int, RB: Int): Int = if (RA == B) 1 else 0
  }

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

  val sampleStrings = scala.io.Source.fromFile("samples").getLines.toList.filter(!_.isEmpty)

  case class Sample(before: Registers, input: List[Int], after: Registers)
  {
    def cabBeCausedBy(instruction: Instruction): Boolean = instruction.apply(input, before) == after
    def opCode: Int = input.head
  }

  object Sample
  {
    def parse(l: List[String]): Sample = {
      require(l.length == 3)

      def toInts(s: String): List[Int] =
        s.dropWhile(!_.isDigit).takeWhile(_ != ']').filter(_ != ',').split(' ').map(_.toInt).toList
      def asRegisters(x: List[Int]): Registers = (0 to 3).zip(x).toMap

      val before = toInts(l(0))
      val input = toInts(l(1))
      val after = toInts(l(2))

      Sample(asRegisters(before), input, asRegisters(after))
    }
  }


  val samples = sampleStrings.grouped(3).map(Sample.parse).toList
  val result1 = samples.count(sample => Instructions.count(sample.cabBeCausedBy) >= 3)
  println(s"result 1: $result1") // 677

  // part2
  var opCodeMap: Map[Int, Instruction] = Map()
  val samplesByOpcode = samples.groupBy(_.opCode)
  while (opCodeMap.size < 16)
  {
    samplesByOpcode.filter(oc => !opCodeMap.contains(oc._1)).foreach { case (opcode, smpls) if !opCodeMap.contains(opcode) =>
      // find all instructions that match all samples and arent already in the map
      val canBecaused = Instructions.filter(i => smpls.forall(s => s.cabBeCausedBy(i)))

      val candidates = canBecaused.filter(i => !opCodeMap
        .values
        .map(_.getClass.getName).exists(_ == i.getClass.getName))

      // if more than 1, skip
      if (candidates.size == 1)
        opCodeMap = opCodeMap.updated(opcode, candidates.head)
      else if (candidates.isEmpty) throw new RuntimeException("0 candidates")
    }
  }

  // print op code map
  opCodeMap.toList.sortBy(_._1).foreach { case (opcode, instruction) =>
    println(f"$opcode%2d -> ${instruction.getClass.getSimpleName.toUpperCase}")}

  val program = scala.io.Source.fromFile("program").getLines

  var registers = (0 to 3).map(_ -> 0).toMap
  program.foreach(line => {
    val nl = line.split(' ').map(_.toInt).toList
    registers = opCodeMap(nl.head).apply(nl, registers)
  })

  val result2 = registers(0)
  println(s"result2: $result2")
}
