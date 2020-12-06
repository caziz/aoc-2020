package day4
import scala.io.Source

object Day4 extends App {
  val source = Source.fromFile("src/day4/input.txt")
  val fields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")
  val passports = source.mkString
    .split("\n\n")
    .map {
      _.split("[ \n]")
        .map { case s"$k:$v" => k -> v }
        .toMap
    }
    .filter(_.keySet + "cid" == fields)

  val part1 = passports.length
  println(s"part1=$part1")

  val part2 = passports.count {
    val nums = "([0-9]*)".r
    val nums4 = "([0-9]{4})".r
    val nums9 = "([0-9]{9})".r
    val hex = "(#[0-9a-f]{6})".r
    _.forall {
      case "byr" -> nums4(byr) => (1920 to 2002) contains byr.toInt
      case "iyr" -> nums4(iyr) => (2010 to 2020) contains iyr.toInt
      case "eyr" -> nums4(eyr) => (2020 to 2030) contains eyr.toInt
      case "hgt" -> s"${nums(hgt)}cm" => (150 to 193) contains hgt.toInt
      case "hgt" -> s"${nums(hgt)}in" => (59 to 76) contains hgt.toInt
      case "hcl" -> hex(_) => true
      case "ecl" -> ("amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth") => true
      case "pid" -> nums9(_) => true
      case "cid" -> _ => true
      case _ => false
    }
  }
  println(s"part2=$part2")
}
