package jcanero.day1

import jcanero.utils._

import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap

object DayOnePartOne {
  def main(args: Array[String]) {
    val b = HashSet.newBuilder[Int]
    InputReader
      .getPuzzleInput(1, args)
      .foreach(b += _.toInt)
    val s = b.result()

    val target = 2020

    for (v <- s) {
      val needle = target - v
      if (s.contains(needle)) {
        println(v * needle)
        return
      }
    }
  }
}

object DayOnePartTwo {
  def main(args: Array[String]) {
    val target = 2020
    val b1 = HashSet.newBuilder[Int]
    val b2 = HashMap.newBuilder[Int, Int]
    InputReader
      .getPuzzleInput(1, args)
      .foreach(is => {
        val i = is.toInt
        b1 += i
        b2 += ((target - i, i))
      })
    val numberSet = b1.result()
    val subtractMap = b2.result()

    for (v <- numberSet) {
      for ((sum, item) <- subtractMap) {
        val needle = sum - v
        if (numberSet.contains(needle)) {
          println(v * item * needle)
          return
        }
      }
    }
  }
}
