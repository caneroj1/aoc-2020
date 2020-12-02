package jcanero.utils

import scala.io._

object InputReader {
  def getPuzzleInput(day: Int, part: Int): Iterator[String] = {
    implicit val c = Codec.defaultCharsetCodec
    Source.fromFile(s"input/day${day}/part${part}.txt").getLines()
  }
}
