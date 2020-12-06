package day3
import scala.io.Source

object Day3 extends App {
  val source = Source.fromFile("src/day3/input.txt")
  val lines = source.getLines.toSeq

  val countTrees = (right: Int, down: Int) =>
    lines.grouped(down).map(_.head).zipWithIndex.count {
        case (line, i) =>
          line((i * right) % line.length) == '#'
      }

  val part1 = countTrees(3, 1)
  println(s"part1=$part1")

  val slopes = Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
  val part2 = slopes.map(countTrees.tupled).product
  println(s"part2=$part2")
}
