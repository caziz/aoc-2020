package day11
import common.ArrayUtils._
import scala.io.Source

object Day11 extends App {
  val source = Source.fromFile("src/day11/input.txt")
  val grid = source.getLines.map(_.toCharArray).toArray
  type Grid = Array[Array[Char]]

  val dirs = for {
    dx <- -1 to 1
    dy <- -1 to 1
    if dx != 0 || dy != 0
  } yield (dx, dy)

  def countVisible(grid: Grid, i: Int, j: Int): Int =
    dirs.count {
      case (dx, dy) =>
        Iterator.iterate((i + dx, j + dy)) {
          case (i, j) => (i + dx, j + dy)
        }.map {
          case (i, j) => grid.lift2D(i, j)
        }.dropWhile(_.contains('.')).next.contains('#')
    }

  def countNeighbors(grid: Grid, i: Int, j: Int): Int =
    dirs.count {
      case (dx, dy) => grid.lift2D(i + dx, j + dy).contains('#')
    }

  def mkNextGrid(count: (Grid, Int, Int) => Int, maxCount: Int)(grid: Grid): Grid =
    grid.zipWithIndex2D.map2D {
      case (c, i, j) => (c, count(grid, i , j)) match {
        case ('L', 0) => '#'
        case ('#', n) if n >= maxCount => 'L'
        case _ => c
      }
    }

  def fixedPoint(grid: Grid, nextGrid: Grid => Grid): Grid =
    Iterator.iterate(grid)(nextGrid)
      .sliding(2).find {
        case Seq(prev, next) => prev.sameElements2D(next)
      }.get.head

  val nextGrid1 = mkNextGrid(countNeighbors, 4) _
  val part1 = fixedPoint(grid, nextGrid1).count2D(_ == '#')
  println(s"part1=$part1")

  val nextGrid2 = mkNextGrid(countVisible, 5) _
  val part2 = fixedPoint(grid, nextGrid2).count2D(_ == '#')
  println(s"part2=$part2")
}
