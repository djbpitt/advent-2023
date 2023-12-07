/** AoC 2023, Day 7
 *
 */

import scala.io.Source

object Cards extends Enumeration {
  type Card = Value
  val C2, C3, C4, C5, C6, C7, C8, C9, CT, CJ, CQ, CK, CA = Value
}
import Cards._
object Hands extends Enumeration {
  type Hand = Value
  val HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind = Value
}
import Hands._

/** Convert Char from hand to enum card value
 *
 * E.g., A => CA, 4 => C4
 *
 * @param input Char as single character of hand
 * @return String that prepends 'C' to the input
 */
def cardify(input: Char): Card =
  Cards.withName("C" + input.toString)

def handify(input: Vector[Card]): Map[Card, Int] =
  val grouped = input.groupBy(identity).map((key, value) => key -> value.length)
  grouped

def handType(input: Map[Card, Int]): Hand =
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
@main def t(): Unit =
  val rawData = Source.fromResource("12-07_data_test.txt").mkString("").linesIterator
  rawData.foreach(println)

//  val handInput: String = "7AQAQ"
//  val cards: Vector[Card] = handInput.map(cardify).toVector
//  val hand: Map[Card, Int] = handify(cards)
//  println(hand)
//  println(handType(hand))
//  val testHands: Vector[String] = Vector("AA8AA", "23332", "TTT98", "23432", "A23A4", "23456", "32T3K", "T55J5", "KK677", "KTJJT", "QQQJA")
//  val results = testHands.map(_.map(cardify).toVector).map(handify).map(handType)
//  testHands.zip(results).foreach(println)
//  println(results.sorted)
//  println(List(results.max, results.min))
