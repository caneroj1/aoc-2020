package jcanero.day2

import jcanero.utils._

case class ParsedPassword(range: (Int, Int), needle: Char, password: String)

object PasswordParser {
  def parse(input: String): Option[ParsedPassword] = {
    val tokens = input.split(" ")
    if (tokens.length != 3) {
      println(s"Password '${input}' is not valid")
      return None
    }

    val minMax = tokens(0).split("-")
    val min = minMax(0).toInt
    val max = minMax(1).toInt

    val char = tokens(1)(0)

    val password = tokens(2)
    Some(ParsedPassword((min, max), char, password))
  }
}

object DayTwoPartOne {
  def isPasswordValid(input: String): Boolean = {
    PasswordParser.parse(input) match {
      case None => false
      case Some(ParsedPassword((min, max), needle, password)) => {
        val count = password.count(_ == needle)
        count >= min && count <= max
      }
    }
  }

  def main(args: Array[String]) {
    val numberValid = InputReader
      .getPuzzleInput(2, 1)
      .count(isPasswordValid _)
    println(numberValid)
  }
}

object DayTwoPartTwo {
  def isPasswordValid(input: String): Boolean = {
    PasswordParser.parse(input) match {
      case None => false
      case Some(ParsedPassword((pos1, pos2), needle, password)) => {
        password(pos1 - 1) == needle && password(pos2 - 1) != needle ||
        password(pos1 - 1) != needle && password(pos2 - 1) == needle
      }
    }
  }

  def main(args: Array[String]) {
    val numberValid = InputReader
      .getPuzzleInput(2, 1)
      .count(isPasswordValid _)
    println(numberValid)
  }
}
