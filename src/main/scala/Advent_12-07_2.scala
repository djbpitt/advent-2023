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
 * Can’t just increase index because three of a kind promotes to four of a kind, not full house
 * Recursive because two pairs promotes to three of a kind, which then skips full house to go to four of a kind
 *
 * @param in    original Hand
 * @param steps number of jokers
 * @return promoted Hand
 */
def promote(in: Hand, steps: Int): Hand =
  @tailrec
  def promoteStep(hand: Hand, count: Int): Hand = // increase hand type by steps recursively
    if count == 0 then hand // no more jokers
    else if hand == ThreeOfAKind then // skip full house
      promoteStep(FourOfAKind, count - 1)
    else // next step in order
      promoteStep(Hands(hand.id + 1), count - 1)

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

/** Map from card type to count of cards of that type
 *
 * Possible improvements:
 * Add joker count of 0 where missing to simplify testing for presence of absence later
 * Rename function (currently "handify") because it returns card counts but not hand type
 *
 * @param input Vector[Card] with five cards in hand
 * @return map from card types to counts
 */
private def handify(input: Vector[Card]): Map[Card, Int] =
  val grouped = input
    .groupBy(identity)
    .map((key, value) => key -> value.length)
  grouped

/** Determine hand type from card counts
 *
 * Does not yet adapt type for jokers
 *
 * @param input Map[Card, Int] with counts of each card type in hand
 * @return Hand
 */
private def handType(input: Map[Card, Int]): Hand =
  input.values.max match {
    case 5 => FiveOfAKind
    case 4 => FourOfAKind
    case 3 => input.size match {
      case 2 => FullHouse
      case _ => ThreeOfAKind
    }
    case 2 => input.size match {
      case 3 => TwoPair
      case _ => OnePair
    }
    case _ => HighCard
  }
@main def main7(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-07_data_test.txt").getLines.toVector // :+ "JJJJJ 10000"
  val rawCardInput: Vector[(String, Int)] = rawInput.map(parseHandData)
  val cardsByHand: Vector[Vector[Card]] = rawCardInput.map(e => e._1.map(cardify).toVector)
  val hands: Vector[Map[Card, Int]] = cardsByHand.map(handify)
  val handTypes: Vector[Hand] = hands.map(handType)
  val handsAndTypes: Vector[(Vector[Card], Hand)] = cardsByHand
    .zip(handTypes)
  val promotedTypes: Vector[Hand] = handsAndTypes.map {
    case (_, hand) if hand == FiveOfAKind => FiveOfAKind
    case (cards, hand) if cards.contains(CJ) =>
      val jokerCount = cards.groupBy(identity)(CJ).length
      promote(hand, jokerCount)
    case (_, hand) => hand
  }
  val bets: Vector[Int] = rawCardInput.map((_, f) => f)
  val cardDataInstances = cardsByHand
    .zip(promotedTypes)
    .zip(bets)
    .map(e => CardData(e._1._1, e._1._2, e._2))
  // rank (low to high, one-based) is offset + 1
  val sortedCardDataInstances: Vector[CardData] = cardDataInstances.sortBy(e => (e.HandType, e.Cards))
  sortedCardDataInstances.foreach(println)
  val amountWon = sortedCardDataInstances
    .zipWithIndex
    .map((data, rank) => data.Bet * (rank + 1)).sum
  println(s"Part 2: $amountWon")

case class CardData(Cards: Vector[Card], HandType: Hand, Bet: Int)

