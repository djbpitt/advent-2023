package Advent_12_13

import scala.annotation.tailrec
import scala.util.Using
import scala.io.Source
import scala.util.matching.Regex

// https://hackernoon.com/manachers-algorithm-explained-longest-palindromic-substring-22cb27a5e96f
@main def main13(): Unit =
  /* Setup */
  val rawLines = Source.fromResource("12-13_data_test.txt")
    .getLines()
    .mkString("\n")
  val tasks = rawLines.split("\n\n").toVector.map(_.split("\n").map(_.toVector).toVector)
  /* In both original and transposed form:
  *
  * 1. Insert spacers to force all row and column counts to be odd
  * 2. Shape into vector of strings
  * 3. Find all palindromes (and their lengths) in all strings
  * 4. Find all aligned palindrome centers (non-zero values array of lengths)
  * 5. Find all shared alignments and select the longest (?)
  * 6. Adjust for insertions
  * 7. Value is sum of number of rows above center in original
  *    and 10 times the number of rows above center in transposed
  * */
  println("Original")
  tasks.head.foreach(println)
  println("Transposed")
  tasks.head.transpose.foreach(println)








