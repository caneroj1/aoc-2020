package jcanero.day3

import jcanero.utils._

object DayThreeHelpers {
  sealed trait Tile

  case object Tree extends Tile
  case object Empty extends Tile

  def charToTile(c: Char): Tile = c match {
    case '.' => Empty
    case '#' => Tree
  }

  def getMap(args: Array[String]): Vector[Vector[Tile]] = {
    InputReader
      .getPuzzleInput(3, args)
      .map(_.map(charToTile _).toVector)
      .toVector
  }
}

object DayThreePartOne {
  import DayThreeHelpers._
  def main(args: Array[String]) = {
    val map = getMap(args)
    var startColumn = 0
    var treesHit = 0
    for (row <- Range(0, map.length)) {
      val currentRow = map(row)
      val inc = currentRow(startColumn) match {
        case Tree  => 1
        case Empty => 0
      }
      treesHit += inc
      startColumn = (startColumn + 3) % currentRow.length
    }
    println(treesHit)
  }
}

object DayThreePartTwo {
  import DayThreeHelpers._

  case class Position(row: Int, col: Int)
  case class Slope(
      position: Option[Position],
      increment: Position,
      treeCount: Int
  ) {
    def canAdvance(): Boolean = !position.isEmpty
  }

  def advanceSlope(map: Vector[Vector[Tile]])(slope: Slope): Slope = {
    slope.position match {
      case None => slope
      case Some(Position(row, col)) => {
        val currentRow = map(row)

        val inc = currentRow(col) match {
          case Tree  => 1
          case Empty => 0
        }

        val newCount = slope.treeCount + inc
        val newRow = row + slope.increment.row
        if (newRow < map.length) {
          val newCol = (col + slope.increment.col) % currentRow.length
          Slope(Some(Position(newRow, newCol)), slope.increment, newCount)
        } else {
          Slope(None, slope.increment, newCount)
        }
      }
    }
  }

  def main(args: Array[String]) = {
    val map = getMap(args)
    var slopes = Array(
      Slope(Some(Position(0, 0)), Position(1, 1), 0),
      Slope(Some(Position(0, 0)), Position(1, 3), 0),
      Slope(Some(Position(0, 0)), Position(1, 5), 0),
      Slope(Some(Position(0, 0)), Position(1, 7), 0),
      Slope(Some(Position(0, 0)), Position(2, 1), 0)
    )

    var stillSlopes = true
    while (stillSlopes) {
      slopes.partition(_.canAdvance()) match {
        case (Array(), _) => stillSlopes = false
        case (validSlopes, invalidSlopes) => {
          slopes = validSlopes.map(advanceSlope(map)).concat(invalidSlopes)
        }
      }
    }

    val treeCounts = slopes.map(_.treeCount.longValue())
    treeCounts.foreach(println)

    val product = treeCounts.product
    println(product)
  }
}
