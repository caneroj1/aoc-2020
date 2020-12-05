package jcanero.day5

import jcanero.utils._
import scala.annotation.tailrec

object DayFiveHelpers {
  @tailrec def traverseSpec(spec: String, min: Int, max: Int): Int = {
    spec.splitAt(1) match {
      case (s, rest) if s == "F" || s == "L" =>
        traverseSpec(rest, min, ((min + max) / 2.0).floor.toInt)
      case (s, rest) if s == "B" || s == "R" =>
        traverseSpec(rest, ((min + max) / 2.0).ceil.toInt, max)
      case _ => min
    }
  }

  def toSeatId(input: String): Long = {
    input.splitAt(7) match {
      case (rowSpec, colSpec) => {
        val row = traverseSpec(rowSpec, 0, 127)
        val col = traverseSpec(colSpec, 0, 7)
        row * 8 + col
      }
    }
  }
}

object DayFivePartOne {
  import DayFiveHelpers._

  def main(args: Array[String]) {
    val seatId = InputReader
      .getPuzzleInput(5, args)
      .map(toSeatId)
      .max
    println(seatId)
  }
}

object DayFivePartTwo {
  import DayFiveHelpers._

  def main(args: Array[String]) {
    val seatIds = InputReader
      .getPuzzleInput(5, args)
      .map(toSeatId)
      .toVector
      .sorted

    val missing = seatIds
      .zip(seatIds.tail)
      .map({ case (x, y) => (x, y - x) })
      .filter(_._2 > 1)

    if (missing.length != 1) {
      println("Could not determine missing seat")
    } else {
      println(missing(0)._1 + 1)
    }
  }
}
