package day6
import scala.io.Source

object Day6 extends App {
  val source = Source.fromFile("src/day6/input.txt")
  val questions = source.mkString.split("\n\n").map(_.split("\n").map(_.toSet))

  val part1 = questions.map(_.reduce(_ union _).size).sum
  println(s"part1=$part1")

  val part2 = questions.map(_.reduce(_ intersect _).size).sum
  println(s"part2=$part2")
}

