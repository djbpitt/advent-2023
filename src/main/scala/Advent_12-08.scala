/** Day 8
 *
 * Part 2 requires lowest common multiple
 */

import scala.annotation.tailrec
import scala.io.Source

/** Parse instruction line into map entries for nested map
 *
 * @param direction String, instruction line (e.g., AAA = (BBB, CCC))
 * @return Nested map, where outer key points to inner map with L and R keys
 */
private def parsePointer(direction: String): (String, Map[Char, String]) =
  direction match {
    case s"${key} = (${left}, ${right})" =>
      key -> Map('L' -> left, 'R' -> right)
  }

/** Read data from specified file, split into directions (LR …) and strings containing path steps
 *
 * @param filename String
 * @return Tuple2 of LR line and steps as map items
 */
private def readData(filename: String): (String, Vector[String]) =
  val rawData: Vector[String] = Source.fromResource(filename).getLines().toVector
  val directions: String = rawData.head
  val mapItems: Vector[String] = rawData.drop(2)
  (directions, mapItems)

/** Part 1: find ZZZ
 *
 * Recursive steps, accumulating count
 *
 * @param directions Vector[Char], sequence of L and R values
 * @param pointers Map[String, Map[Char, String]], nested map, where outer key has L and R children
 * @return Int number of steps to reach ZZZ key
 */
private def findZZZ(directions: Vector[Char], pointers: Map[String, Map[Char, String]]) =
  val directionsLength: Int = directions.length

  @tailrec
  def takeStep(stepNo: Int, stepStart: String, direction: Char): Int =
    val nextStepStart: String = pointers(stepStart)(direction)
    if nextStepStart == "ZZZ" then
      stepNo
    else
      val nextStepNo: Int = stepNo + 1
      val nextDirection: Char = directions(stepNo % directionsLength)
      takeStep(nextStepNo, nextStepStart, nextDirection)

  takeStep(1, "AAA", directions.head)

/** Steps until each path reaches a value ending in "Z"
 *
 * @param directions Vector[Char] (LR …)
 * @param pointers   Map from string to L/R, which point to strings
 * @return number of steps as Int
 */
private def findXXZ(directions: Vector[Char], pointers: Map[String, Map[Char, String]]): Set[Int] =
  val directionsLength: Int = directions.length

  @tailrec
  def takeStep(stepNo: Int, stepStart: String, direction: Char): Int =
    val nextStepStart: String = pointers(stepStart)(direction)
    if nextStepStart.endsWith("Z") then
      stepNo
    else
      val nextStepNo: Int = stepNo + 1
      val nextDirection: Char = directions(stepNo % directionsLength)
      takeStep(nextStepNo, nextStepStart, nextDirection)

  val initialStepStarts: Set[String] = pointers.keySet.filter(_.endsWith("A"))
  val result: Set[Int] = initialStepStarts.map(e => takeStep(stepNo = 1, stepStart = e, directions.head))
  result

/** Lowest common multiple
 *
 * https://stackoverflow.com/questions/40875537/fp-lcm-in-scala-in-1-line
 *
 * @param list Longs for which to compute lcm
 * @return Long representing lcm
 */
private def lcm(list: Iterable[Long]): Long =
  @tailrec def loop(x: Long, y: Long): Long =
    if (y == 0) x else loop(y, x % y)

  list.foldLeft(1L): (a, b) =>
    b * a / loop(a, b)

private def main8_1(directions: Vector[Char], pointers: Map[String, Map[Char, String]]): Int =
  val result = findZZZ(directions, pointers)
  result

/** Number of path steps to arrive at all destinations simultaneously
 *
 * Lowest common multiple of all lengths is the first place the 6 values synchronize
 *
 * @param directions Vector[Char] (LR …)
 * @param pointers   map from start (as string) to L/R (as char) to target (as string)
 * @return least common multiple of 6 path lengths
 */
private def main8_2(directions: Vector[Char], pointers: Map[String, Map[Char, String]]): Long =
  val lengths: Set[Int] = findXXZ(directions, pointers) // Lengths of each path (6 values)
  val result = lcm(lengths.map(_.toLong).toIndexedSeq) // Lowest common multiple of all lengths
  result

@main def main8(): Unit =
  // Setup
  val rawData = readData("12-08_data.txt")
  val directions: Vector[Char] = rawData.head.toVector // LR …
  val pointerData: Vector[String] = rawData.last
  val pointers = pointerData.map(parsePointer).toMap
  // Part 1
  val result1: Int = main8_1(directions, pointers)
  println(s"Part 1: $result1")
  // Part 2
  val result2: Long = main8_2(directions, pointers)
  println(s"Part 2: $result2")
