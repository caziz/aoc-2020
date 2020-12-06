package day2
import scala.io.Source

object Day2 extends App {
  val source = Source.fromFile("src/day2/input.txt")
  val lines = source.getLines.map {
    case s"$lo-$hi $c: $word" =>
      (lo.toInt, hi.toInt, c.head, word)
  }.toSeq

  val part1 = lines.count {
    case (lo, hi, c, word) =>
      val count = word.count(_ == c)
      (lo to hi).contains(count)
  }
  println(s"part1=$part1")

  val part2 = lines.count {
    case (lo, hi, c, word) =>
      (word(lo - 1) == c) ^ (word(hi - 1) == c)
  }
  println(s"part2=$part2")
}
