package day12
import scala.io.Source

object Day12 extends App {
  val source = Source.fromFile("src/day12/input.txt")
  val actionRegex = raw"(.)(\d+)".r
  val actions = source.getLines.map {
    case actionRegex(action, value) => (action.head, value.toInt)
  }.toSeq

  case class Ship(wx: Int = 1, wy: Int = 0, isWaypoint: Boolean = false, x: Int = 0, y: Int = 0) {
    def turn(degrees: Int): Ship =
      (360 + degrees) % 360 match {
        case 90 => copy(wx = wy, wy = -wx)
        case n => turn(90).turn(n - 90)
      }

    def takeAction(action: (Char, Int)): Ship =
      action match {
        case ('N', n) if isWaypoint => copy(wy = wy + n)
        case ('E', n) if isWaypoint => copy(wx = wx + n)
        case ('S', n) if isWaypoint => copy(wy = wy - n)
        case ('W', n) if isWaypoint => copy(wx = wx - n)

        case ('N', n) => copy(y = y + n)
        case ('E', n) => copy(x = x + n)
        case ('S', n) => copy(y = y - n)
        case ('W', n) => copy(x = x - n)

        case ('L', n) => turn(-n)
        case ('R', n) => turn(n)
        case ('F', n) => copy(x = x + n * wx, y = y + n * wy)
    }

    def takeActions(actions: Seq[(Char, Int)]): Ship =
      actions.foldLeft(this) {
        case (ship, action) => ship.takeAction(action)
      }
    def manhattan: Int = x.abs + y.abs
  }

  val part1 = Ship().takeActions(actions).manhattan
  println(s"part1=$part1")

  val part2 = Ship(10, 1, isWaypoint = true).takeActions(actions).manhattan
  println(s"part2=$part2")
}
