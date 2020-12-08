package day8
import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {
  val source = Source.fromFile("src/day8/input.txt")
  val instrs = source.getLines.map { case s"$instr $num" => (instr, num.toInt) }.toSeq

  case class State(mem: Seq[(String, Int)],
                   pic: Int = 0,
                   acc: Int = 0,
                   executed: Set[Int] = Set(),
                   repeated: Boolean = false,
                   halted: Boolean = false) {
    @tailrec
    final def execute: State =
      if (pic == mem.size)
        copy(halted = true)
      else if (executed contains pic)
        copy(repeated = true)
      else {
        val default = copy(pic = pic + 1, executed = executed + pic)
        val state = mem(pic) match {
          case ("acc", n) => default.copy(acc = acc + n)
          case ("jmp", n) => default.copy(pic = pic + n)
          case ("nop", _) => default
        }
        state.execute
      }
  }

  val part1 = State(instrs).execute.acc
  println(s"part1=$part1")

  val instrsList = instrs.zipWithIndex.collect {
    case (("jmp", n), i) => (("nop", n), i)
    case (("nop", n), i) => (("jmp", n), i)
  }.map {
    case (instr, i) => instrs.take(i) ++ Seq(instr) ++ instrs.drop(i + 1)
  }
  val part2 = instrsList.map(State(_).execute).find(_.halted).get.acc
  println(s"part2=$part2")
}