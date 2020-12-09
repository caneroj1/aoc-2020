package jcanero.day7

import jcanero.utils._

import scala.collection.mutable.Set
import scala.collection.mutable.SortedMap
import scala.collection.mutable.Queue
import scala.collection.immutable.Map

object ParseUtils {
  type Style = String
  type ParentStyle = String

  case class BagSpec(style: Style, count: Int)
  case class BagLine(parent: ParentStyle, bags: Array[BagSpec])

  def parseChildren(childrenSpec: String): Array[BagSpec] = {
    childrenSpec
      .split(", ")
      .map(_ match {
        case s"${count} ${style} bag"   => BagSpec(style, count.toInt)
        case s"${count} ${style} bag."  => BagSpec(style, count.toInt)
        case s"${count} ${style} bags"  => BagSpec(style, count.toInt)
        case s"${count} ${style} bags." => BagSpec(style, count.toInt)
      })
  }

  def toBagLine(line: String): BagLine = {
    line.split(" contain ") match {
      case Array(s"${parentStyle} bags", children) => {
        if (children == "no other bags.") {
          BagLine(parentStyle, Array.empty)
        } else {
          BagLine(parentStyle, parseChildren(children))
        }
      }
    }
  }
}

object DaySevenPartOne {
  import ParseUtils._
  type BagLineage = Map[Style, Set[ParentStyle]]

  def toBagLineage(bagLines: Iterator[BagLine]): BagLineage = {
    val m: SortedMap[Style, Set[ParentStyle]] = SortedMap.empty
    bagLines.foreach(l => {
      l.bags.foreach(b => {
        m.updateWith(b.style) {
          case None => Some(Set.from(Iterable.single(l.parent)))
          case Some(oldSet) => {
            oldSet.add(l.parent)
            Some(oldSet)
          }
        }
      })
    })
    m.toMap
  }

  def bagsContaining(lineage: BagLineage, style: Style): Int = {
    var count = 0
    val q = Queue.empty[Style]
    val checked = Set.empty[Style]
    q.addOne(style)

    while (!q.isEmpty) {
      val styleToCheck = q.dequeue()
      if (!checked.contains(styleToCheck)) {
        checked.add(styleToCheck)
        lineage.get(styleToCheck) match {
          case None          =>
          case Some(parents) => q.addAll(parents)
        }
      }
    }
    checked.size - 1
  }

  def main(args: Array[String]) {
    val lines = InputReader
      .getPuzzleInput(7, args)
      .map(toBagLine)
    val lineage = toBagLineage(lines)
    val containing = bagsContaining(lineage, "shiny gold")
    println(containing)
  }
}

object DaySevenPartTwo {
  import ParseUtils._

  type BagGraph = Map[Style, Array[BagSpec]]

  def toBagGraph(bagLines: Iterator[BagLine]): BagGraph = {
    bagLines.map(l => (l.parent, l.bags)).toMap
  }

  def countIndividualBags(bagGraph: BagGraph, style: Style): Int = {
    var count = 0
    val q = Queue.empty[(Int, Style)]
    q.addOne((1, style))
    while (!q.isEmpty) {
      val (amount, currentStyle) = q.dequeue()
      bagGraph.get(currentStyle) match {
        case Some(children) => {
          val numberOfChildren = children.map(_.count).sum
          count += amount * numberOfChildren
          q.addAll(children.map(c => (amount * c.count, c.style)))
        }
        case None =>
      }
    }
    count
  }

  def main(args: Array[String]) {
    val lines = InputReader
      .getPuzzleInput(7, args)
      .map(toBagLine)
    val g = toBagGraph(lines)
    val count = countIndividualBags(g, "shiny gold")
    println(count)
  }
}
