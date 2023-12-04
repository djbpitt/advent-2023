import os.*

import scala.annotation.tailrec
import scala.util.matching.Regex

// Group 1 is card number, 2 is winners, 3 is values to check
val linePattern: Regex = """^Card\s+(\d+):\s+(.+)\s+\|\s+(.+)$""".r

private def parseLine(dataLine: String): (Int, String, String) =
  val linePattern(cardNo, winners, toCheck) = dataLine
  (cardNo.toInt, winners, toCheck)

@main def main4(): Unit =
  // Setup
  val dataLines = os.read(os.pwd / "src" / "resources" / "12-04_data.txt")
    .split("\n")
  val splitLines = dataLines.map(parseLine)
  splitLines.foreach(println)
