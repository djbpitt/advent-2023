import os.*

import scala.util.matching.Regex
import annotation.tailrec

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

private def scoreCard(winCount: Int): Int =
  winCount match {
    case 0 => 0
    case e => scala.math.pow(2, e - 1).toInt
  }

private def main4_1(cardScores: Vector[Int]): Unit =
  println(cardScores.sum)

private def main4_2(winCounts: Vector[Int]): Unit =
  // cardNo, winCount, score
  @tailrec
  def computeCardCounts(offset: Int, cardReps: Vector[Int]): Vector[Int] =
    if offset == winCounts.size then cardReps
    else
      // Update cardReps here
      computeCardCounts(offset + 1, cardReps)
  val cardReps = computeCardCounts(offset = 0, cardReps = Vector.fill(winCounts.size)(1))
  println(cardReps.size)

@main def main4(): Unit =
  // Setup
  val dataLines = os.read(os.pwd / "src" / "resources" / "12-04_data.txt")
    .split("\n")
  val splitLines = dataLines.map(parseLine) // (cardNo: Int, winners: String, toCheck: String)
  val winCounts = splitLines.map(e => countWins(e._2, e._3)).toVector // number of winners as Int
  val cardScores = winCounts.map(scoreCard)
  main4_1(cardScores)
  main4_2(winCounts)
