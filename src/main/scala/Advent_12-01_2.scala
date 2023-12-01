import os.*

import scala.annotation.tailrec
import scala.language.postfixOps // needed for .r regex pattern

/** Task is underspecified
 * nineight (with overlap) resolves to 98 (the overlap counts for both)
 * */
val wordToChar: Map[String, Char] = Map(
  "one" -> '1',
  "two" -> '2',
  "three" -> '3',
  "four" -> '4',
  "five" -> '5',
  "six" -> '6',
  "seven" -> '7',
  "eight" -> '8',
  "nine" -> '9'
)
@main def main2(): Unit =
  // Construct pattern to match number word at beginning dynamically
  val digitPattern = ("^(" + wordToChar.keys.mkString("|") + ")").r
  val input = os.read(os.pwd/"src"/"resources"/"12-01_data.txt")
    .split("\n")
  @tailrec
  /** Find all numbers, whether digits or words
   *
   * @param item : input line as CharArray
   * @param accumulator : holds all digits found as Vector[Char]
   *
   * @return Vector[Char] of all digits found
   * */
  def findDigits(item: Array[Char], accumulator: Vector[Char]): Vector[Char] =
    if item.isEmpty then
      accumulator
    else
      val first = item.head
      item match {
        case _ if first.isDigit =>
          // Array.drop(n) slices to end without knowing length
          findDigits(item.drop(1), accumulator :+ first)
        case _ =>
          // Need to convert CharArray to String for regex matching
          val digitAsWord = digitPattern.findAllIn(item.mkString)
          if digitAsWord.isEmpty then
            findDigits(item.drop(1), accumulator)
          else
            // There must be exactly one if we get this far, and the lookup must succeed
            val word = digitAsWord.toVector.head
            val digit = wordToChar(word)
            findDigits(item.drop(1), accumulator :+ digit)
      }
  val results = input
    .map(_.toCharArray) // Convert to CharArray early, since that’s where we spend the most time
    .map(findDigits(_, Vector()))
    .map(e => e.head.asDigit * 10 + e.last.asDigit) // Treat Char as Int
    .sum
  println(results)









