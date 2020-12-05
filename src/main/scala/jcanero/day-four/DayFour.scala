package jcanero.day4

import jcanero.utils._
import scala.collection.immutable.Map
import scala.collection.immutable.Vector
import scala.collection.mutable.Builder

object DayFourHelpers {
  type Validator = String => Boolean
  type Validators = Map[String, Validator]

  def defaultValidator(s: String) = true

  case class Passport(properties: Map[String, String]) {
    def isValid(validators: Validators): Boolean = {
      val validKeys = validators.keySet
      if (properties.keySet.intersect(validKeys).size != validKeys.size) {
        return false
      }

      validators.forall({ case (key, fn) => fn(properties(key)) })
    }
  }

  def parseLine(
      line: String,
      builder: Builder[(String, String), Map[String, String]]
  ) {
    builder.addAll(
      line
        .split(" ")
        .map(_.split(":") match {
          case Array(k, v) => (k, v)
        })
    )
  }

  def parsePassports(input: Iterator[String]): Vector[Passport] = {
    val b = Vector.newBuilder[Passport]
    while (input.hasNext) {
      val currentPropertiesBuilder = Map.newBuilder[String, String]
      for (line <- input.takeWhile(!_.isEmpty())) {
        parseLine(line, currentPropertiesBuilder)
      }
      b.addOne(Passport(currentPropertiesBuilder.result()))
    }
    b.result()
  }
}

object DayFourPartOne {
  import DayFourHelpers._
  def main(args: Array[String]) {
    val validKeys = Map(
      "byr" -> defaultValidator _,
      "iyr" -> defaultValidator _,
      "eyr" -> defaultValidator _,
      "hgt" -> defaultValidator _,
      "hcl" -> defaultValidator _,
      "ecl" -> defaultValidator _,
      "pid" -> defaultValidator _
    )
    val passports = parsePassports(InputReader.getPuzzleInput(4, args))
    val invalidCount = passports.filter(_.isValid(validKeys)).length
    println(invalidCount)
  }
}

object DayFourPartTwo {
  import DayFourHelpers._
  def main(args: Array[String]) {
    def validateByr(v: String): Boolean = {
      v.toIntOption match {
        case None    => false
        case Some(i) => i >= 1920 && i <= 2002
      }
    }

    def validateIyr(v: String): Boolean = {
      v.toIntOption match {
        case None    => false
        case Some(i) => i >= 2010 && i <= 2020
      }
    }

    def validateEyr(v: String): Boolean = {
      v.toIntOption match {
        case None    => false
        case Some(i) => i >= 2020 && i <= 2030
      }
    }

    def validateHgt(v: String): Boolean = {
      v match {
        case s"${value}cm" =>
          value.toIntOption match {
            case None    => false
            case Some(i) => i >= 150 && i <= 193
          }
        case s"${value}in" =>
          value.toIntOption match {
            case None    => false
            case Some(i) => i >= 59 && i <= 76
          }
        case _ => false
      }
    }

    def validateHcl(v: String): Boolean = {
      "#[0-9a-f]{6}".r.matches(v)
    }

    def validateEcl(v: String): Boolean = {
      v == "amb" ||
      v == "blu" ||
      v == "brn" ||
      v == "gry" ||
      v == "grn" ||
      v == "hzl" ||
      v == "oth"
    }

    def validatePid(v: String): Boolean = {
      "[0-9]{9}".r.matches(v)
    }

    val validKeys = Map(
      "byr" -> validateByr _,
      "iyr" -> validateIyr _,
      "eyr" -> validateEyr _,
      "hgt" -> validateHgt _,
      "hcl" -> validateHcl _,
      "ecl" -> validateEcl _,
      "pid" -> validatePid _
    )
    val passports = parsePassports(InputReader.getPuzzleInput(4, args))
    val invalidCount = passports.filter(_.isValid(validKeys)).length
    println(invalidCount)
  }
}
