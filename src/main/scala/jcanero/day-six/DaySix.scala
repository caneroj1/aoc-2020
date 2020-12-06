package jcanero.day6

import jcanero.utils._
import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ReusableBuilder

object DaySixPartOne {
  case class Folder(
      currentSum: Long,
      currentGroup: ReusableBuilder[Char, HashSet[Char]]
  )

  def main(args: Array[String]) {
    val result = InputReader
      .getPuzzleInput(6, args)
      .foldLeft(Folder(0, HashSet.newBuilder)) {
        case (Folder(sum, builder), "") => {
          val distinctQuestions = builder.result().size
          Folder(sum + distinctQuestions, HashSet.newBuilder)
        }
        case (Folder(sum, builder), questions) => {
          Folder(sum, builder.addAll(questions.toCharArray()))
        }
      } match {
      case Folder(sum, finalBuilder) => sum + finalBuilder.result().size
    }
    println(result)
  }
}

object DaySixPartTwo {
  case class Folder(
      currentSum: Long,
      groupSize: Long,
      currentGroup: HashMap[Char, Long]
  ) {
    def countOfYesToAll(): Long =
      currentGroup.valuesIterator.filter(_ == groupSize).length
  }

  def main(args: Array[String]) {
    val result = InputReader
      .getPuzzleInput(6, args)
      .foldLeft(Folder(0, 0, HashMap.empty)) {
        case (f, "") => {
          Folder(f.currentSum + f.countOfYesToAll, 0, HashMap.empty)
        }
        case (Folder(sum, size, m), questions) => {
          questions
            .toCharArray()
            .foreach(c => m.update(c, m.getOrElse(c, 0L) + 1))
          Folder(sum, size + 1, m)
        }
      } match {
      case f => f.currentSum + f.countOfYesToAll
    }
    println(result)
  }
}
