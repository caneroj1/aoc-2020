package jcanero.day10

import jcanero.utils._

object DayTenPartOne {
  def main(args: Array[String]) {
    val inputs = InputReader
      .getPuzzleInput(10, args)
      .map(_.toInt)
      .toArray
      .sorted
    val diffs = inputs
      .zip(inputs.tail)
      .map({ case (l, r) => r - l })
    var diffsOfThree = 1
    var diffsOfOne = 0

    inputs(0) match {
      case 1 => diffsOfOne += 1
      case 2 =>
      case 3 => diffsOfThree += 1
      case _ => {
        println("Invalid adapters!")
        return
      }
    }
    diffsOfOne += diffs.count(_ == 1)
    diffsOfThree += diffs.count(_ == 3)

    println(diffsOfOne * diffsOfThree)
  }
}
