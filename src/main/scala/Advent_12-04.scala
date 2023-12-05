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

private def computeMultipliers(winCounts: Vector[Int], cardScores: Vector[Int]): Vector[Int] =
  @tailrec
  def computeMultiplierStep(offset: Int, cardReps: Vector[Int]): Vector[Int] =
//    if offset == winCounts.size then cardReps
    if offset == winCounts.size then cardReps
    else
      val winnersAtOffset: Int = winCounts(offset) // Determines how many games to augment
      // If Card at position 0 has 4 winners, hit positions 1 through 5 = 0 + 1 through (exclusive) 0 + 4 + 1
      val countsToAugment: Vector[Int] = cardReps.slice(offset + 1, offset + winnersAtOffset + 1)
      val augmentedCounts: Vector[Int] = countsToAugment.map(_ + cardReps(offset))
      val newCardReps: Vector[Int] = cardReps.take(offset + 1) :++ augmentedCounts :++ cardReps.drop(offset + winnersAtOffset + 1)
      // println(s"Offset: $offset, Winners: $winnersAtOffset, New reps: $newCardReps")
      computeMultiplierStep(offset + 1, newCardReps)
  val multipliers: Vector[Int] = computeMultiplierStep(offset = 0, cardReps = Vector.fill(winCounts.size)(1))
  multipliers

private def main4_1(cardScores: Vector[Int]): Unit =
  val result = cardScores.sum
  println(s"Part 1: $result")

private def main4_2(winCounts: Vector[Int], cardScores: Vector[Int]): Unit =
  val multipliers: Vector[Int] = computeMultipliers(winCounts, cardScores)
  val result = winCounts.zip(multipliers).zip(cardScores).map(e => (e._1._1, e._1._2, e._2))
  // Number of winners | Multiplier | Card score
  val total = result.map(_._2).sum // Asks for count of cards, not scores
  println(s"Part 2: $total")

@main def main4(): Unit =
  // Setup
  val data = os.read(os.pwd / "src" / "resources" / "12-04_data.txt")
//  val data = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
//               |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
//               |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
//               |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
//               |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
//               |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin
  val dataLines = data.split("\n")
  val splitLines = dataLines.map(parseLine) // (cardNo: Int, winners: String, toCheck: String)
  val winCounts = splitLines.map(e => countWins(e._2, e._3)).toVector // number of winners as Int
  val cardScores = winCounts.map(scoreCard)
  main4_1(cardScores)
  main4_2(winCounts, cardScores)
