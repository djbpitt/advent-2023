import os.*

import scala.annotation.tailrec
import scala.language.postfixOps

/** Problem is underspecified
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
  val digitPattern = ("^(" + wordToChar.keys.mkString("|") + ")").r
  val input = os.read(os.pwd/"src"/"resources"/"12-01_data.txt")
    .split("\n")
  @tailrec
  def findDigits(item: String, accumulator: Vector[Char]): Vector[Char] =
    if item.isEmpty then
      accumulator
    else
      val first = item.toCharArray.head
      item match {
        case _ if first.isDigit =>
          findDigits(item.substring(1), accumulator :+ first)
        case _ =>
          val digitAsWord = digitPattern.findAllIn(item)
          if digitAsWord.isEmpty then
            findDigits(item.substring(1), accumulator)
          else
            val word = digitAsWord.toVector.head
            val digit = wordToChar(word)
            findDigits(item.substring(1), accumulator :+ digit)
      }
  val results = input
    .map(findDigits(_, Vector()))
    .map(e => e.head.asDigit * 10 + e.last.asDigit).sum
  println(results)









