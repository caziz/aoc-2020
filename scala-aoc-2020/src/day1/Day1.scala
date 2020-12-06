package day1
import scala.io.Source

object Day1 extends App {
  val source = Source.fromFile("src/day1/input.txt")
  val nums = source.getLines.map(_.toInt).toSeq

  val part1 = nums.combinations(2).find(_.sum == 2020).get.product
  println(s"part1=$part1")

  val part2 = nums.combinations(3).find(_.sum == 2020).get.product
  println(s"part2=$part2")
}
