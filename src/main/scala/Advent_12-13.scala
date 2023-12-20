package Advent_12_13

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

/** Find all palindromes
 *
 * 1. Insert spacers to force all row and column counts to be odd
 * 2. Find all palindromes (and their lengths) in all strings
 * 3. Find all aligned palindrome centers (non-zero values array of lengths)
 * 4. Find all shared alignments and select the longest (?)
 * 5. Adjust for insertions
 *
 * @param in search space as vector of chars
 * @return vector of ints, representing length of one side of palindrome centered at that position
 */
private def manacher(in: Vector[Char]) =
  // Insert spacers
  val paddingChar: Char = '*'
  val padded: Vector[Char] = in.flatMap(e => Vector(paddingChar, e)) :+ paddingChar
  // Initialize result vector
  val radii = collection.mutable.ArrayBuffer.fill(padded.size)(0) // counts out from center, with center not included
  // Recursive traversal goes here
  // If i > r (new palindrome extends beyond r), i = c and reset l and r
  // If i < r and its mirror < l, minimum radii(i) is r - i
  // If i < r and its mirror >= l, minimum radii(i) is the mirror value (could be longer)
  for i <- 0 until padded.size do
    radii(i) = i
  radii



// https://hackernoon.com/manachers-algorithm-explained-longest-palindromic-substring-22cb27a5e96f
// https://github.com/int8/Manacher-Algorithm-in-Scala
@main def main13(): Unit =
  /* Setup */
  val rawLines = Source.fromResource("12-13_data_test.txt")
    .getLines()
    .mkString("\n")
  val tasks = rawLines.split("\n\n").toVector.map(_.split("\n").map(_.toVector).toVector)
  tasks.map(_.map(manacher)).foreach(println)







