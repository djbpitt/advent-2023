/** AoC 2023, Day 6, Part 2
 *
 * Can be solved by a) brute force, b) with quadratic equations, or c) with binary search
 *
 * Quadratic equations (https://www.cuemath.com/algebra/solving-quadratic-equations/)
 * 1. Convert to standard form: ax^2 + bx + c = 0 (with 0 on right side)
 * 2. Quadratic formula: x = (-b ± sqrt(b^2 - 4ac))/2a, then simplify
 * 3. Discriminant (D): b^2 - 4ac:
 *    D > 0 => two real and distinct roots
 *    D = 0 => one real root
 *    D < 0 => two distinct complex roots
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


private def binarySearch(race: Race) =
  // y = max * x - x^2
  val threshold: Long = race.distance
  @tailrec
  def bisect(x: Long, min: Long, max: Long, oldY: Long): Long =
    def computeY(x: Long): Long = (max - x) * x
    val y = computeY(x)
    y match {
      case y if (y - oldY) < 1 => Vector((x - 1, computeY(x - 1)), (x, computeY(x)), (x + 1, computeY(x + 1))).filter(_._2 <= threshold).max._1
      case y if y > race.distance => bisect(x = (x - min) / 2, min = min, max = x, y)
      case y if y < race.distance => bisect(x = (max - x) / 2, min = x, max = max, y)
      case _ => x
    }
  // Start at mid-x of ascending parabola;
  val max = race.time
  bisect(x = race.time / 2, min = 0, max = race.time, oldY = 0)


private def main6_1(): Unit =
  val stats: Vector[Race] = parseInput_1("12-06_data.txt")
  // val stats: Vector[Race] = parseInput_1("12-06_data_test.txt")
  val winnerCounts = stats.map(e => computeTimeDistanceMapping(e).length)
  val winnerProduct = winnerCounts.product
  println(s"Part 1: $winnerProduct")

private def main6_2(): Unit =
  /*
   * Downward parabola of Total * Press - Press^2
   * Vertex at Press / 2
   * Values are continuous, so solve for one side and mirror
   */
  val stats: Race = parseInput_2("12-06_data.txt")
  println(stats) // single Race object, with time and target distance to beat
  // Brute-force
  val winnerCounts = computeTimeDistanceMapping(stats).length
  println(s"Part 2 (brute force): $winnerCounts")
  // Quadratic equation


@main def main6(): Unit =
  main6_1()
  main6_2()

case class Race(time: Long, distance: Long)
