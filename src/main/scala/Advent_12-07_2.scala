package Advent_12_07_2

/** AoC 2023, Day 7, part 2
 *
 * Separate package because reuses names
 *
 */

import scala.io.Source

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

private def parseHandData(rawHandInput: String): (String, Int) =
  rawHandInput match {
    case s"${handString} ${bet}" => (handString, bet.toInt)
    case _ => (s"ERROR: $rawHandInput", -1)
  }

/** Convert Char from hand to enum card value
 *
 * E.g., A => CA, 4 => C4
 *
 * @param input Char as single character of hand
 * @return String that prepends 'C' to the input
 */
private def cardify(input: Char): Card =
  Cards.withName("C" + input.toString)

private def promoteJoker(oldHand: Map[Card, Int]) =
  oldHand match {
    case e if e.keySet.contains(CJ) && e(CJ) == 5 => oldHand
    case _ =>
      val newHandBasis = oldHand.-(CJ) // Remove J to find most frequent other card
        .toSeq
        .sortBy((card, count) => (count, card))
      val bestCard = newHandBasis
        .groupBy((card, count) => count)
        .toSeq.maxBy((count, cardData) => (count, cardData.head))
        ._2 // List of frequent card types
        .last // Most frequent
        ._1 // Card type
      println(bestCard)
      oldHand
  }

private def handify(input: Vector[Card]): Map[Card, Int] =
  val grouped = input.groupBy(identity).map((key, value) => key -> value.length)
  grouped

private def handType(input: Map[Card, Int]): Hand =
  val counts = input.values
  input match {
    case h if counts.max == 5 => FiveOfAKind
    case h if counts.max == 4 => FourOfAKind
    case h if counts.max == 3 && input.size == 2 => FullHouse
    case h if counts.max == 3 && input.size == 3 => ThreeOfAKind
    case h if counts.max == 2 && input.size == 3 => TwoPair
    case h if counts.max == 2 && input.size == 4 => OnePair
    case h if counts.max == 5 => HighCard
    case _ => new RuntimeException("oops"); HighCard
  }
@main def main7(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-07_data_test.txt").getLines.toVector :+ "JJJJJ 10000"
  val rawCardInput: Vector[(String, Int)] = rawInput.map(parseHandData)
  val cardsByHand: Vector[Vector[Card]] = rawCardInput.map(e => e._1.map(cardify).toVector)
  val hands: Vector[Map[Card, Int]] = cardsByHand.map(handify)
  val promotedHands = hands.map(promoteJoker)
  hands.zip(promotedHands).foreach(println)
  val handTypes: Vector[Hand] = hands.map(handType)
  val bets = rawCardInput.map((e, f) => f)
  val cardDataInstances = cardsByHand
    .zip(handTypes)
    .zip(bets)
    .map(e => CardData(e._1._1, e._1._2, e._2))
  // rank (low to high, one-based) is offset + 1
  val sortedCardDataInstances: Vector[CardData] = cardDataInstances.sortBy(e => (e.HandType, e.Cards))
  //  sortedCardDataInstances.foreach(println)
  val amountWon = sortedCardDataInstances.zipWithIndex.map((data, rank) => data.Bet * (rank + 1)).sum

//  println(amountWon)
case class CardData(Cards: Vector[Card], HandType: Hand, Bet: Int)

