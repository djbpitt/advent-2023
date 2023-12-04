import os.*

import scala.util.matching.Regex

// Group 1 is card number (not used), 2 is winners, 3 is values to check
// Some apparent spaces are really tabs, so use \s instead of literal space character
val linePattern: Regex = """^Card\s+(\d+):\s+(.+)\s+\|\s+(.+)$""".r

/** parseLine()
 *
 * Split line into card number, winning values, and values to check
 *
 * @param dataLine line to parse
 * @return card number as Int, winners and toCheck as strings
 */
private def parseLine(dataLine: String): (Int, String, String) =
  val linePattern(cardNo, winners, toCheck) = dataLine
  (cardNo.toInt, winners, toCheck)

/** countWins()
 *
 * Determine number of winning values in values to check
 * Assumes (correctly) that values to check are unique for each line
 *
 * @param winners winners as string, to split
 * @param toCheck values to check as string ,to split
 * @return count of winning values to check
 */
private def countWins(winners: String, toCheck: String): Int =
  val winnersSet = winners.split("\\s+").toSet
  val toCheckSet = toCheck.split("\\s+").toSet
  (winnersSet intersect toCheckSet).size

@main def main4(): Unit =
  // Setup
  val dataLines = os.read(os.pwd / "src" / "resources" / "12-04_data.txt")
    .split("\n")
  val splitLines = dataLines.map(parseLine) // (cardNo: Int, winners: String, toCheck: String)
  val winCounts = splitLines.map(e => countWins(e._2, e._3)) // number of winners as Int
  val scores = winCounts map {
    case 0 => 0
    case e => scala.math.pow(2, e - 1).toInt
  }
  println(scores.sum)