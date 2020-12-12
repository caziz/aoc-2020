package day9
import scala.io.Source

object Day9 extends App {
  val source = Source.fromFile("src/day9/input.txt")
  val nums = source.getLines.map(_.toLong).toSeq

  val part1 = nums.sliding(26).find {
    case slice :+ last =>
      slice.combinations(2).forall(_.sum != last)
  }.get.last
  println(s"part1=$part1")

  val lists = for {
    start <- nums.indices
    end <- start until nums.size
  } yield (start, end)

  val list = lists.map {
    case (start, end) => nums.slice(start, end)
  }.find(_.sum == part1).get

  val part2 = list.min + list.max
  println(s"part2=$part2")
}