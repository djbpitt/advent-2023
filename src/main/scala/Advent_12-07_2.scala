package Advent_12_07_2

/** AoC 2023, Day 7, part 2
 *
 * Separate package because reuses names
 */

import scala.io.Source
import annotation.tailrec

private object Cards extends Enumeration {
  // J (Joker) has lowest rank
  type Card = Value
  val CJ, C2, C3, C4, C5, C6, C7, C8, C9, CT, CQ, CK, CA = Value
}
import Cards._

private object Hands extends Enumeration {
  type Hand = Value
  val HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind = Value
}
import Hands._

/** Method to promote Hand type for jokers
 *
 * Will not overrun with natural data
 *
 * @param i       Int from 0 to end in declared order (low to high)
 * @param name    String equal to object name
 * @param promote Promote hand type
 */
protected case class Hands(i: Int, name: String, promote: Hand)

/** Increase hand type by number of jokers
 *
 * Can’t just increase index because one pair promotes to three of a kind, and not to two pair
 *
 * @param in    original Hand
 * @param steps number of jokers
 * @return promoted Hand
 */
def promote(in: Hand, steps: Int): Hand =
  @tailrec
  def promoteStep(hand: Hand, count: Int): Hand = // increase hand type by steps recursively
    (hand, count) match {
      case (hand, 0) => hand // no jokers
      case (hand, 5) => FiveOfAKind // all jokers
      case (FourOfAKind, _) => promoteStep(FiveOfAKind, count - 1)
      case (ThreeOfAKind, _) => promoteStep(FourOfAKind, count -1)
      case (TwoPair, _) => promoteStep(FullHouse, count -1)
      case (OnePair, _) => promoteStep(ThreeOfAKind, count -1)
      case (HighCard, _) => promoteStep(OnePair, count - 1)
    }

  val promoted: Hand = promoteStep(in: Hand, steps: Int) // launch recursion
  promoted

private def parseHandData(rawHandInput: String): (String, Int) =
  rawHandInput match {
    case s"${handString} ${bet}" => (handString, bet.toInt)
    case _ => (s"ERROR: $rawHandInput", -1)
  }

/** Convert Char from hand to enum Card (Value)
 *
 * E.g., A => CA, 4 => C4
 *
 * @param input Char as single character of hand
 * @return String that prepends 'C' to the input
 */
private def cardify(input: Char): Card =
  Cards.withName("C" + input.toString)

/** Map from card types to count of cards of each type
 *
 * @param input Vector[Card] with five cards in hand
 * @return map from card types to counts
 */
private def computeCardCounts(input: Vector[Card]): Map[Card, Int] =
  val grouped: Map[Card, Int] = input
    .groupBy(identity)
    .map((key, value) => key -> value.length)
  grouped

/** Determine hand type from card counts
 *
 * Does not yet adapt type for jokers
 *
 * @param input Map[Card, Int] with counts of each card type in hand
 * @return tuple of Hand type without jokers and joker count
 */
private def handType(input: Map[Card, Int]): (Hand, Int) =
  val cardCountsWithoutJokers: Map[Card, Int] = input match
    case e if e.contains(CJ) => e.removed(CJ)
    case e => e
  val jokerCount: Int = 5 - cardCountsWithoutJokers.values.sum
  val handTypeWithoutJokers = cardCountsWithoutJokers
    .values
    .toList
    .sorted
    .reverse match {
    case 5 :: _ => FiveOfAKind
    case 4 :: _ => FourOfAKind
    case 3 :: 2 :: _ => FullHouse
    case 3 :: _ => ThreeOfAKind
    case 2 :: 2 :: _ => TwoPair
    case 2 :: _ => OnePair
    case _ => HighCard
  }
  (handTypeWithoutJokers, jokerCount)
@main def main7(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-07_data.txt").getLines.toVector // :+ "JJJJJ 10000"
  val rawCardInput: Vector[(String, Int)] = rawInput.map(parseHandData)
  val cardsByHand: Vector[Vector[Card]] = rawCardInput.map(e => e._1.map(cardify).toVector)
  val hands: Vector[Map[Card, Int]] = cardsByHand.map(computeCardCounts)
  val bets: Vector[Int] = rawCardInput.map(_._2)
  val handTypeData: Vector[CardData] = hands
    .map(handType)
    .map(promote)
    .zip(cardsByHand).zip(bets)
    .map(e => (e._1._1, e._1._2, e._2))
    .map(e => CardData(handType = e._1, cards = e._2, bet=e._3))
    .sortBy(e => (e.handType, e.cards))
  handTypeData.foreach(println)
  val amountWon = handTypeData
    .zipWithIndex
    .map((data, rank) => data.bet * (rank + 1))
    .sum
  rawCardInput.foreach(println)
  println(s"Part 2: $amountWon")

case class CardData(handType: Hand, cards: Vector[Card], bet: Int)

