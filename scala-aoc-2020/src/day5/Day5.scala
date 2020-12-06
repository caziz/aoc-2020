package day5
import scala.io.Source

object Day5 extends App {
  val source = Source.fromFile("src/day5/input.txt")

  val seatIds = source.getLines.map(_.map {
    case 'F' | 'L' => '0'
    case 'B' | 'R' => '1'
  }).map(Integer.parseInt(_, 2)).toSeq

  val part1 = seatIds.max
  println(s"part1=$part1")

  val allSeatIds = (0 until seatIds.min) ++ seatIds ++ ((seatIds.max + 1) until (1 << 10))
  val part2 = allSeatIds.reduce(_ ^ _)
  println(s"part2=$part2")
}

