package Advent_12_13

import scala.io.Source

/** Find all palindromes (Manacher's algorithm)
 *
 * 1. Insert spacers to force all row and column counts to be odd
 * 2. Find all palindromes (and their lengths) in all strings
 * 3. Find all aligned palindrome centers
 *
 * Returns the lengths of the palindromes at each position. We don't care about the specific length, but we
 *   do care whether all lengths at that position are the same
 *
 * Ported from: https://hackernoon.com/manachers-algorithm-explained-longest-palindromic-substring-22cb27a5e96f
 * See also: https://github.com/int8/Manacher-Algorithm-in-Scala
 *
 * @param in search space as vector of chars
 * @return vector of ints, representing length of one side of palindrome centered at that position
 */
private def manacher(in: Vector[Char]): Vector[Int] =
  // Insert spacers
  val paddingChar: Char = '*'
  val padded: Vector[Char] = in.flatMap(e => Vector(paddingChar, e)) :+ paddingChar
  // Initialize
  var c: Int = 0
  var r: Int = 0
  val radii = collection.mutable.ArrayBuffer.fill(padded.size)(0) // counts out from center, with center not included
  // If i > r (new palindrome extends beyond r), i = c and reset l and r
  // If i < r and its mirror < l, minimum radii(i) is r - i
  // If i < r and its mirror >= l, minimum radii(i) is the mirror value (could be longer)
  for i <- padded.indices do
    val mirrorIndex = (2 * c) - i
    if i < r then radii(i) = List(r - i, radii(mirrorIndex)).min // minimum radius based on mirror
    var newL = i - (1 + radii(i))
    var newR = i + (1 + radii(i))
    while newR < padded.size && newL >= 0 && padded(newL) == padded(newR) do
      radii(i) += 1
      newL -= 1
      newR += 1
    if i + radii(i) > r then
      c = i
      r = i + radii(i)
  radii.toVector

/** Filter manacher output to find centers
 *
 * Centers of reflection can be identified because all values in the vector are the same
 *
 * NB: Centers only valid if between rows or columns, i.e., only a space between rows
 *   or columns, and not a row or column itself, can be the center
 *
 * @param in vector of vectors of palindrome lengths by position, one inner vector per row or column
 * @return zero or one position of reflection (TODO: currently may return more than one)
 */
def findAlignment(in: Vector[Vector[Int]]) =
  in
    .transpose // easier to work with rows
    .zipWithIndex // we care about position, and not the actual value
    .filter((radii, _) => radii.forall(_ == radii.head)) // early exit, so faster than measuring the size of a (deduplicated) set
    .filter((radii, _) => radii.head > 1) // values or 0 or 1 aren't meaningful
    .filter((radii, _) => radii.head % 2 == 0) // even value means between rows or columns
    .map((_, pos) => pos) // return only the position

/** Compute row or column score for task
 * 
 * Compute both; one will be zero
 * 
 * @param task matrix as vector of vector of chars
 * @return int representing (necessarily non-zero) score for that task according to the instructions
 */
def scoreTask(task: Vector[Vector[Char]]): Int =
  val rowScore =  findAlignment(task.map(manacher)) match {
    case e if e.isEmpty => 0 // no reflection on this dimension
    case e => e.head / 2 // halve to account for padding
  }
  val colScore = findAlignment(task.transpose.map(manacher)) match {
    case e if e.isEmpty => 0 // no reflection on this dimension
    case e => e.head / 2 // halve to account for padding
  }
  rowScore + 100 * colScore

@main def main13(): Unit =
  /* Read data and separate groups of lines into tasks (vector of vector of chars) */
  val rawLines = Source.fromResource("12-13_data.txt")
    .getLines()
    .mkString("\n")
  /* Map each task to vector of vector of chars, wrap in vector */
  val tasks: Vector[Vector[Vector[Char]]] = rawLines
    .split("\n\n")
    .toVector
    .map(_.split("\n")
      .map(_.toVector)
      .toVector)
  /* Total scores for all tasks */
  val totalScore = tasks.map {e =>
    val result = scoreTask(e)
    result
  }.sum
  println(s"Part 1 result: $totalScore")







