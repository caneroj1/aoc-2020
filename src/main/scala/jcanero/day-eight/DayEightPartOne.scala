package jcanero.day8

import jcanero.utils._

import scala.collection.immutable.Set

object DayEightHelpers {
  sealed trait Instruction
  case class NoOp(by: Int) extends Instruction
  case class Acc(by: Int) extends Instruction
  case class Jmp(by: Int) extends Instruction

  def parseInstruction(line: String): Instruction = {
    line match {
      case s"nop ${n}" => NoOp(n.toInt)
      case s"acc ${n}" => Acc(n.toInt)
      case s"jmp ${n}" => Jmp(n.toInt)
    }
  }
}

object DayEightPartOne {
  import DayEightHelpers._

  def main(args: Array[String]) {
    val instructions = InputReader
      .getPuzzleInput(8, args)
      .map(parseInstruction)
      .toArray
    val instructionCounter = Array.ofDim[Int](instructions.length)
    var shouldIterate = true
    var currentInstruction = 0
    var acc = 0
    while (shouldIterate) {
      if (instructionCounter(currentInstruction) > 0) {
        shouldIterate = false
      } else {
        instructionCounter(currentInstruction) = 1
        instructions(currentInstruction) match {
          case NoOp(by) => currentInstruction += 1
          case Acc(by) => {
            acc += by
            currentInstruction += 1
          }
          case Jmp(by) => currentInstruction += by
        }
      }
    }

    println(acc)
  }
}

object DayEightPartTwo {
  import DayEightHelpers._

  /*
  try brute force but memoize when something has been traversed
  execute normally, on first no-op or jmp, reverse it and continue to traverse
  and mark things as hit, and stop when you hit a loop. if you hit a jmp or no-op
  that was traversed already but you didn't flip it, do the flip and continue tthe traverse.

  do recursively:
  - store array of locations that were traversed already
  - tracker array needs to be an immutable that changes per recursive call - NO


  - start at 0
  - execute instructions
   */
  def traverse(
      instructions: Array[Instruction],
      instructionCounter: Array[Int],
      currentTraversal: Set[Int],
      flipped: Array[Boolean],
      instructionIndex: Int,
      canFlip: Boolean,
      accumulator: Int
  ): Option[Int] = {
    if (instructionIndex >= instructions.length) {
      return Some(accumulator)
    }

    if (instructionIndex < 0) {
      return None
    }

    instructions(instructionIndex) match {
      case NoOp(by) => {
        if (instructionCounter(instructionIndex) == 0) {
          instructionCounter(instructionIndex) = 1
          traverse(
            instructions,
            instructionCounter,
            currentTraversal + instructionIndex,
            flipped,
            instructionIndex + 1,
            canFlip,
            accumulator
          ) match {
            case r @ Some(_) => return r
            case None        =>
          }
        }

        if (
          canFlip && !flipped(instructionIndex) && !currentTraversal.contains(
            instructionIndex
          )
        ) {
          flipped(instructionIndex) = true
          traverse(
            instructions,
            instructionCounter,
            currentTraversal + instructionIndex,
            flipped,
            instructionIndex + by,
            false,
            accumulator
          ) match {
            case r @ Some(_) => return r
            case None        =>
          }
        }

        None
      }
      case Acc(by) => {
        if (instructionCounter(instructionIndex) == 0) {
          instructionCounter(instructionIndex) = 1
          return traverse(
            instructions,
            instructionCounter,
            currentTraversal + instructionIndex,
            flipped,
            instructionIndex + 1,
            canFlip,
            accumulator + by
          )
        }
      }
      case Jmp(by) => {
        if (instructionCounter(instructionIndex) == 0) {
          instructionCounter(instructionIndex) = 1
          traverse(
            instructions,
            instructionCounter,
            currentTraversal + instructionIndex,
            flipped,
            instructionIndex + by,
            canFlip,
            accumulator
          ) match {
            case r @ Some(_) => return r
            case None        =>
          }
        }

        if (
          canFlip && !flipped(instructionIndex) && !currentTraversal.contains(
            instructionIndex
          )
        ) {
          flipped(instructionIndex) = true
          traverse(
            instructions,
            instructionCounter,
            currentTraversal + instructionIndex,
            flipped,
            instructionIndex + 1,
            false,
            accumulator
          ) match {
            case r @ Some(_) => return r
            case None        =>
          }
        }

        None
      }
    }

    None
  }

  def main(args: Array[String]) {
    val instructions = InputReader
      .getPuzzleInput(8, args)
      .map(parseInstruction)
      .toArray
    val instructionCounter = Array.ofDim[Int](instructions.length)
    val flippedCounter = Array.ofDim[Boolean](instructions.length)
    val traversalSet = Set.empty[Int]
    traverse(
      instructions,
      instructionCounter,
      traversalSet,
      flippedCounter,
      0,
      true,
      0
    ) match {
      case None        => println("No result!")
      case Some(value) => println(value)
    }
  }
}
