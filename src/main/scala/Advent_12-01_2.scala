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
  def findDigits(item: String, accumulator: Vector[Char]): Vector[Char] =
    item match { // Lower-case "string" is variable, not datatype
      case string if string.isEmpty => accumulator // Exit condition, string is depleted
      case string if string(0).isDigit => // Found a literal digit
        findDigits(item.drop(1), accumulator :+ string(0))
      case string => // Might or might not be a number word
        digitPattern.findAllIn(string) match { // Possibly empty match iterator
          case seq if seq.nonEmpty => // Found a number word
            val digit = wordToChar(seq.matched) // Matched string in match iterator (if any)
            findDigits(item.drop(1), accumulator :+ digit)
          case _ => findDigits(item.drop(1), accumulator) // Found nothing useful
        }
    }
  val results = input
    .map(findDigits(_, Vector()))
    .map(e => e.head.asDigit * 10 + e.last.asDigit) // Treat Char as Int
    .sum
  println(results)









