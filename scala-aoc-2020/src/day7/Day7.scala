package day7

import scala.annotation.tailrec
import scala.io.Source

object Day7 extends App {
  val source = Source.fromFile("src/day7/input.txt")
  val lines = source.getLines.toSeq

  type BagLookUp = Map[Bag, List[(Int, Bag)]]
  case class Bag(color: String) {
    def canHold(bag: Bag)(implicit bags: BagLookUp): Boolean =
      bags(this).exists { case (_, b) => b == bag || b.canHold(bag) }

    def bagSize(implicit bags: BagLookUp): Int =
      bags(this).map { case (num, b) => (num * b.bagSize) + num }.sum
  }

  val bagRegex = "(.*) bags?".r
  val bagsRegex = "(\\d*) (.*) bags?".r
  implicit val bags: BagLookUp = lines.map {
    case s"${bagRegex(outerBag)} contain $innerBags." =>
      val innerBagCounts = innerBags match {
        case "no other bags" => Nil
        case list => list
          .split(", ")
          .map { case bagsRegex(num, innerBag) => (num.toInt, Bag(innerBag)) }
          .toList
      }
      Bag(outerBag) -> innerBagCounts
  }.toMap

  val shinyGold = Bag("shiny gold")
  val part1 = bags.keySet.count(_.canHold(shinyGold))
  println(s"part1=$part1")

  val part2 = shinyGold.bagSize
  println(s"part2=$part2")
}