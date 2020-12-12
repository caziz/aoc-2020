package day10
import scala.collection.mutable
import scala.io.Source

object Day10 extends App {
  val source = Source.fromFile("src/day10/input.txt")
  val nums = source.getLines.map(_.toInt).toSeq

  val builtIn = nums.max + 3
  val chain = (nums :+ 0 :+ builtIn).sorted
  val diffs = chain.sliding(2).map { case List(n, m) => m - n }.toSeq
  val part1 = diffs.count(_ == 1) * diffs.count(_ == 3)
  println(s"part1=$part1")

  def memo[A, B](f: A => B): A => B = new mutable.HashMap[A, B]() {
    override def apply(a: A): B = getOrElseUpdate(a, f(a))
  }

  val ways: Int => Long = memo { n =>
    if (!chain.contains(n)) 0
    else if (n == 0) 1
    else (1 to 3).map(n - _).map(ways).sum
  }

  val part2 = ways(builtIn)
  println(s"part2=$part2")
}
