package jcanero.day11

import jcanero.utils._
import scala.annotation.tailrec
import util.control.Breaks._
import jcanero.day11.DayElevenHelpers.Floor
import jcanero.day11.DayElevenHelpers.TakenSeat
import jcanero.day11.DayElevenHelpers.EmptySeat

object DayElevenHelpers {
  type Grid = Vector[Vector[Square]]

  sealed trait Square

  case class Floor() extends Square
  case class TakenSeat() extends Square
  case class EmptySeat() extends Square

  def toSquare(char: Char): Square = {
    char match {
      case '.' => Floor()
      case 'L' => EmptySeat()
      case '#' => TakenSeat()
    }
  }
}

object DayElevenPartOne {
  import DayElevenHelpers._

  def getSquaresAroundMe(grid: Grid, row: Int, col: Int): Vector[Square] = {
    val b = Vector.newBuilder[Square]
    for (i <- Range(row - 1, row + 2)) {
      for (j <- Range(col - 1, col + 2)) {
        breakable {
          if (i == row && j == col) {
            break
          }
          grid.lift(i) match {
            case Some(r) =>
              r.lift(j) match {
                case Some(value) => b += value
                case None        =>
              }
            case None =>
          }
        }
      }
    }
    b.result()
  }

  def evolve(grid: Grid): Int = {
    val newGridBuilder = Vector.newBuilder[Vector[Square]]
    var changes = 0

    for (i <- Range(0, grid.length)) {
      val row = grid(i)
      val rowBuilder = Vector.newBuilder[Square]
      for (j <- Range(0, row.length)) {
        val currentSquare = grid(i)(j)
        val squares = getSquaresAroundMe(grid, i, j)
        val occupiedCount = squares.count(_ == TakenSeat())
        currentSquare match {
          case TakenSeat() if occupiedCount >= 4 => {
            changes += 1
            rowBuilder += EmptySeat()
          }
          case EmptySeat() if occupiedCount == 0 => {
            changes += 1
            rowBuilder += TakenSeat()
          }
          case x => rowBuilder += x
        }
      }
      newGridBuilder += rowBuilder.result()
    }

    val newGrid = newGridBuilder.result()

    if (changes == 0) {
      newGrid
        .map(
          _.count(s => s == TakenSeat())
        )
        .sum
    } else {
      evolve(newGrid)
    }
  }

  def main(args: Array[String]) {
    val grid: Grid = InputReader
      .getPuzzleInput(11, args)
      .map(_.map(toSquare).toVector)
      .toVector
    val takenSeats = evolve(grid)
    println(takenSeats)
  }
}

object DayElevenPartTwo {
  import DayElevenHelpers._

  def getFirstSeat(
      grid: Grid,
      row: Int,
      col: Int,
      rowDelta: Int,
      colDelta: Int
  ): Option[Square] = {
    var shouldContinue = true
    var currentRow = row + rowDelta
    var currentCol = col + colDelta
    while (shouldContinue) {
      grid.lift(currentRow) match {
        case Some(r) =>
          r.lift(currentCol) match {
            case Some(square) =>
              square match {
                case Floor() => {
                  currentRow += rowDelta
                  currentCol += colDelta
                }
                case v => return Some(v)
              }
            case None => shouldContinue = false
          }
        case None => shouldContinue = false
      }
    }
    None
  }

  def getVisibleSeats(grid: Grid, row: Int, col: Int): Vector[Square] = {
    val deltas = Vector(
      (-1, -1),
      (-1, 0),
      (-1, 1),
      (0, -1),
      (0, 1),
      (1, -1),
      (1, 0),
      (1, 1)
    )
    deltas
      .map({ case (rd, cd) => getFirstSeat(grid, row, col, rd, cd) })
      .filter(_.isDefined)
      .map(_.get)
  }

  def evolve(grid: Grid): Int = {
    val newGridBuilder = Vector.newBuilder[Vector[Square]]
    var changes = 0

    for (i <- Range(0, grid.length)) {
      val row = grid(i)
      val rowBuilder = Vector.newBuilder[Square]
      for (j <- Range(0, row.length)) {
        val currentSquare = grid(i)(j)
        val seats = getVisibleSeats(grid, i, j)
        val occupiedCount = seats.count(_ == TakenSeat())
        currentSquare match {
          case TakenSeat() if occupiedCount >= 5 => {
            changes += 1
            rowBuilder += EmptySeat()
          }
          case EmptySeat() if occupiedCount == 0 => {
            changes += 1
            rowBuilder += TakenSeat()
          }
          case x => rowBuilder += x
        }
      }
      newGridBuilder += rowBuilder.result()
    }

    val newGrid = newGridBuilder.result()

    if (changes == 0) {
      newGrid
        .map(
          _.count(s => s == TakenSeat())
        )
        .sum
    } else {
      evolve(newGrid)
    }
  }

  def main(args: Array[String]) {
    val grid: Grid = InputReader
      .getPuzzleInput(11, args)
      .map(_.map(toSquare).toVector)
      .toVector
    val takenSeats = evolve(grid)
    println(takenSeats)
  }
}
