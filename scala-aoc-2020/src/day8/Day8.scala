package day8
import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {
  val source = Source.fromFile("src/day8/input.txt")
  val instrs = source.getLines.map { case s"$instr $num" => (instr, num.toInt) }.toSeq

  case class State(mem: Seq[(String, Int)],
                   pc: Int = 0,
                   acc: Int = 0,
                   executed: Set[Int] = Set(),
                   repeated: Boolean = false,
                   halted: Boolean = false) {
    @tailrec
    final def execute: State =
      if (pc == mem.size)
        copy(halted = true)
      else if (executed contains pc)
        copy(repeated = true)
      else
        (mem(pc) match {
          case ("acc", n) => copy(pc = pc + 1, acc = acc + n)
          case ("jmp", n) => copy(pc = pc + n)
          case ("nop", _) => copy(pc = pc + 1)
        }).copy(executed = executed + pc)
          .execute
  }

  val part1 = State(instrs).execute.acc
  println(s"part1=$part1")

  val part2 = instrs.map {
    case ("jmp", n) => ("nop", n)
    case ("nop", n) => ("jmp", n)
    case other => other
  }.zipWithIndex.map {
    case (instr, i) => instrs.take(i) ++ Seq(instr) ++ instrs.drop(i + 1)
  }.map(State(_).execute).find(_.halted).get.acc

  println(s"part2=$part2")
}