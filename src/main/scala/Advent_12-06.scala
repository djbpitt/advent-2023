/** AoC 2023, Day 6, Part 2
 *
 * Can be solved by a) brute force, b) with quadratic equations, or c) with binary search
 *
 * Quadratic equations (https://www.cuemath.com/algebra/solving-quadratic-equations/)
 * 1. Convert to standard form: ax^2 + bx + c = 0 (with 0 on right side)
 * 2. Quadratic formula: x = (-b ± sqrt(b^2 - 4ac))/2a, then simplify
 * 3. Discriminant (D): b^2 - 4ac:
 * D > 0 => two real and distinct roots
 * D = 0 => one real root
 * D < 0 => two distinct complex roots
 *
 * For binary search: https://www.geeksforgeeks.org/binary-search-in-scala/
 */

import scala.annotation.tailrec
import scala.io.Source

/** Convert string input for multiple races into Vector[Race]
 *
 * @param filename filename in resources director
 * @return Vector[Race]
 */
private def parseInput_1(filename: String): Vector[Race] =
  val rawData = Source.fromResource(filename).mkString("")
  val dataParts = rawData.split("\n").map(_.split("\\s+"))
  val stats = dataParts
    .head // times
    .zip(dataParts.last) // target distances to beat
    .tail // throw out labels
    .map(e => Race(e._1.toLong, e._2.toLong)) // map each pair to Race() object
  stats.toVector

/** Compute all time/distance results for a race and filter to keep only those greater than target
 *
 * Results can then be multiplied (part 1) or counted (part 2)
 * Brute-force strategy
 *
 * @param record : Race
 * @return Vector[Race]
 */
private def computeTimeDistanceMapping(record: Race): Vector[Race] =
  val mappings =
    for pressTime: Long <- 0L to record.time yield
      val runTime: Long = record.time - pressTime
      Race(pressTime, runTime * pressTime)
  mappings.filter(e => e.distance > record.distance).toVector

/** Strip spaces to merge fragmented data into single Race
 *
 * @param filename filename in resources director
 * @return Race
 */
private def parseInput_2(filename: String): Race =
  val rawData: String = Source.fromResource(filename).mkString("")
  // split() produces an array, so must destructure into an array
  val Array(time, distance) = rawData
    .split("\n")
    .map(_.filter(_.isDigit).toLong) // strip nonDigits and convert remainder to long
  Race(time, distance)


/** Compute number of values between quadratic roots
 *
 * @param race single race that supplies total time and distance to beat
 * @return number of values between roots (exclusive)
 */
private def quadratic(race: Race): Long =
  /*
  Task: pressedTime * (totalTime - pressedTime) = distance, must beat target distance
  Plug in known values: x * (race.time - x) = race.distance
  Solve for y > target distance
    (x * race.time) - x^2 > race.distance
    As general quadratic formula:
      ax^2 + bx + c < 0
    With known values:
      x^2 - race.time * x + race.distance < 0
      a = 1, b = -race.time, c = race.distance
  Formula for discriminant: sqrt(b^2 - 4ac) (distance between roots)
  Formula for roots: x = (-b ± disc)/2a
   */
  val discriminant = math.sqrt(math.pow(race.time, 2) - 4 * race.distance)
  val lowRoot = (race.time - discriminant) / 2
  val highRoot = (race.time + discriminant) / 2
  // Range between roots, but exclusive, so if roots are integers round up (low) or down (high)
  val lowRounded = lowRoot.ceil.toLong
  val lowPoint = if lowRoot == lowRounded then lowRounded + 1 else lowRounded
  val highRounded = highRoot.floor.toLong
  val highPoint = if highRoot == highRounded then highRounded - 1 else highRounded
  highPoint - lowPoint + 1 // add 1 because result is inclusive range

/** Count number of winning values using binary search
 *
 * Compute low end, use symmetry to predict high
 *
 * @param race Race object supplies total time and distance to beat
 * @return number of winning times
 */
private def binarySearch(race: Race): Double =

  /** A match is greater than the target but not by more than 1
   *
   * @param value value to test
   * @param target value must be greater than the target, but not by more than 1
   * @return true if value is smallest whole number greater than target
   */
  def isWithinOne(value: Double, target: Long): Boolean =
    value.ceil > target && value - 1 <= target

  /** Binary search for value
   *
   * @param low bottom of range
   * @param high top of range
   * @return double greater than the target, but not by more than 1
   */
  @tailrec
  def bisect(low: Double, high: Double): Double =
    val middle = low + (high - low) / 2
    val middleDistance = middle * (race.time - middle)
    if isWithinOne(middleDistance, race.distance)
      then middle
    else if middleDistance > race.distance then
      bisect(middle, high)
    else if middle < race.distance then
      bisect(low, middle)
    else // shouldn’t happen
      println(s"Error: $middle")
      new RuntimeException("oops")
      0
  bisect(1, race.time)

private def main6_1(): Unit =
  val stats: Vector[Race] = parseInput_1("12-06_data.txt")
  // val stats: Vector[Race] = parseInput_1("12-06_data_test.txt")
  val winnerCounts = stats.map(e => computeTimeDistanceMapping(e).length)
  val winnerProduct = winnerCounts.product
  println(s"Part 1: $winnerProduct")

private def main6_2(): Unit =
  val stats: Race = parseInput_2("12-06_data.txt")
  println(stats) // single Race object, with time and target distance to beat
  /*
   * Brute-force
   * */
  val winnerCounts = computeTimeDistanceMapping(stats).length
  println(s"Part 2 (brute force): $winnerCounts")
  /*
   * Quadratic equation
   * */
  val quadraticCount = quadratic(stats)
  println(s"Part 2 (quadratic equation): $quadraticCount")
  /*
   * Binary search
   * Symmetrical, so uses low end to compute range
   * */
  val lowDouble = binarySearch(stats)
  val binaryCount = (2 * (stats.time / 2 - lowDouble)).toLong.abs
  println(s"Part 2 (binary search): $binaryCount")
@main def main6(): Unit =
  main6_1()
  main6_2()

case class Race(time: Long, distance: Long)
