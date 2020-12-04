package jcanero.utils

import scala.io._

object InputReader {
  def getPuzzleInput(day: Int, args: Array[String]): Iterator[String] = {
    implicit val c = Codec.defaultCharsetCodec
    val testing = args.map(_.toLowerCase()).contains("test")
    val source = if (testing) {
      Source.fromFile(s"input/day${day}/test.txt")
    } else {
      Source.fromFile(s"input/day${day}/data.txt")
    }
    source.getLines()
  }
}
