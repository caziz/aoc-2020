package day7
import scala.io.Source

object Day7 extends App {
  val source = Source.fromFile("src/day7/input.txt")
  val lines = source.getLines.toSeq

  val getColor = "(.*) bags?".r
  val getNumColor = "(\\d*) (.*) bags?".r

  val bags = lines.map {
    case s"${getColor(outerColor)} contain $innerBags." =>
      val innerCountColors = innerBags match {
        case "no other bags" => Nil
        case list => list
          .split(", ")
          .map { case getNumColor(num, color) => (num.toInt, color) }
          .toList
      }
      outerColor -> innerCountColors
  }.toMap

  def canHold(bag1: String, bag2: String): Boolean =
    bags(bag1).exists { case (_, b) => b == bag2 || canHold(b, bag2) }

  def bagSize(bag: String): Int =
    bags(bag).map { case (num, b) => (num * bagSize(b)) + num }.sum

  val part1 = bags.keySet.count(canHold(_, "shiny gold"))
  println(s"part1=$part1")

  val part2 = bagSize("shiny gold")
  println(s"part2=$part2")
}