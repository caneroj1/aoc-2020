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
  case class Position(row: Int, col: Int)
  case class Slope(
      position: Option[Position],
      increment: Position,
      treeCount: Int
  ) {
    def canAdvance(): Boolean = !position.isEmpty
  }

  // 31696434 wrong
  import DayThreeHelpers._
  def main(args: Array[String]) = {
    val map = getMap(args)
    val slopes = Array(
      Slope(Some(Position(0, 0)), Position(1, 1), 0),
      Slope(Some(Position(0, 0)), Position(1, 3), 0),
      Slope(Some(Position(0, 0)), Position(1, 5), 0),
      Slope(Some(Position(0, 0)), Position(1, 7), 0),
      Slope(Some(Position(0, 0)), Position(2, 1), 0)
    )

    while (slopes.exists(_.canAdvance())) {
      for (i <- Range(0, slopes.length)) {
        val slope = slopes(i)
        slope.position match {
          case None => {}
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
              slopes(i) =
                Slope(Some(Position(newRow, newCol)), slope.increment, newCount)
            } else {
              slopes(i) = Slope(None, slope.increment, newCount)
            }
          }
        }
      }
    }

    val treeCounts = slopes.map(_.treeCount.longValue())
    treeCounts.foreach(println)

    val product = treeCounts.product
    println(product)
  }
}
