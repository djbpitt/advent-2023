package Advent_12_11

import scala.collection.SortedMap
import scala.util.Using
import scala.io.Source

/** Expand rows and columns in matrix
 *
 * Empty rows and columns are duplicated to expand matrix
 * Part 1 only
 *
 * @param in     original matrix as Vector[String]
 * @param scale  number of copies to make if target not found as Int
 * @return expanded matrix as vector of vector of chars
 */
private def expandMatrix(in: Vector[String], scale: Int): Vector[Vector[Char]] =
  val result: Vector[Vector[Char]] = in
    .map(_.toVector)
    .flatMap(e => expandRows(e, scale)) // = map and flatten
    .transpose
    .flatMap(e => expandRows(e, scale))
    .transpose // transpose again so that rows and columns are as in input
  result

/** Duplicate a row if empty, otherwise return unchanged
 *
 * See note on expandMatrix about how target can be omitted
 * Part 1 only
 *
 * @param in     one row as Vector[Char]
 * @param scale  number of times to copy row as Int
 * @return the input plus an additional copy if the row contains no #
 */
private def expandRows(in: Vector[Char], scale: Int): Vector[Vector[Char]] =
  if in.distinct.size > 1 then Vector(in) else Vector.fill(scale)(in)

/** Find all galaxies (# characters) in matrix and return (row, col)
 *
 * @param in matrix as Vector[Vector[Char])
 * @return all locations of galaxies as (row: Int, col: Int)
 */
private def findGalaxies(in: Vector[Vector[Char]], target: Char): Vector[(Int, Int)] =
  val result: Vector[(Int, Int)] = in
    .zipWithIndex
    .flatMap(e => {
      val rowNo: Int = e._2
      val indexes: Vector[Int] = findAllIndexes(e._1, target)
      indexes.map(e => (rowNo, e))
    })
  result

/** Find offset positions of all instances of target Char in sequence
 *
 * @param in     haystack as Vector[Char]
 * @param target needle as Char
 * @return all positions as Vector[Int]
 */
private def findAllIndexes(in: Vector[Char], target: Char): Vector[Int] =
  val result: Vector[Int] = in
    .zipWithIndex
    .filter(e => e._1 == target)
    .map(e => e._2)
  result

/** Find Manhattan distance between two galaxies
 *
 * Manhattan distance is sum of horizontal and vertical distances
 * Use absolute values to avoid having to care about order
 *
 * @param source location of one galaxy as (row: Int, col: Int)
 * @param target location of other galaxy as (row: Int, col: Int)
 * @return Manhattan distance between the two
 */
private def manhattan(source: (Int, Int), target: (Int, Int)): Int =
  val horizontal: Int = (source._1 - target._1).abs
  val vertical: Int = (source._2 - target._2).abs
  val result = horizontal + vertical
  // println(s"Distance between $source and $target is $result")
  result

/** Find all pairs of galaxies in order to compute pairwise manhattan distances (Part 1)
 *
 * @param galaxies locations of galaxies ('#' characters) as Vector[(row: Int, col: Int)]
 * @return all pairwise combinations of locations except (self, self) as Vector[((Int, Int), (Int, Int))]
 */
private def findAllGalaxyPairs(galaxies: Vector[(Int, Int)]) =
  val result = (for {
    source <- galaxies
    target <- galaxies
    // If source row is smaller or (same but source column is smaller)
    if source._1 < target._1 || (source._1 == target._1 && source._2 < target._2)
  } yield (source, target))
    .groupBy((e, _) => e) // group by source galaxy
    .to(SortedMap) // sort by source
    .map((k, v) => (k, v.map(_._2))) // remove source from targets; already sorted
  result

/** Find all rows (or, if transposed, columns) with galaxies
 *
 * Operates on unexpanded, original matrix
 * Rows or columns without galaxies are later expanded; knowing which they are
 *   saves us from having to expand them, which overflows heap memory
 * Part 2 only
 *
 * @param matrix Input data as vector of vector of chars (transposed when checking columns)
 * @param target Character that represents galaxy ('#')
 * @return Vector of indexes of rows (or cols) that contain galaxies (counts and positions not needed)
 */
private def findRowsWithGalaxies(matrix: Vector[Vector[Char]], target: Char): Vector[Int] =
  val result = matrix
    .zipWithIndex // row (or col, if transposed) number
    .map((data, num) => (findAllIndexes(data, target).size, num))
    .filter((dataCount, _) => dataCount != 0)
    .map((_, num) => num)
  result

private def findGalaxies2(matrix: Vector[Vector[Char]], target: Char): Vector[Galaxy] =
  val result = matrix
    .zipWithIndex // row numbers
    .flatMap(e => {
      val rowNo: Int = e._2
      val indexes: Vector[Int] = findAllIndexes(e._1, target)
      indexes.map(e => Galaxy(rowNo, e))
    })
  result

private def findGalaxyPairs2(galaxies: Vector[Galaxy]): SortedMap[Galaxy, Vector[Galaxy]] =
  val result = (for {
    source <- galaxies
    target <- galaxies
    // If source row is smaller or (same but source column is smaller)
    if source.row < target.row || (source.row == target.row && source.col < target.col)
  } yield (source, target))
    .groupBy((e, _) => e) // group by source galaxy
    .to(SortedMap)
    .map((k, v) => (k, v.map(_._2))) // remove source from targets; already sorted
  result

private def expandManhattan(
                             source: Galaxy,
                             target: Galaxy,
                             expansionFactor: Int, // Add **1 less than** this number of empty rows / cols
                             galaxiesToExpand: Map[String, Vector[Int]] // string is row or column, value is indexes of empties
                           ): Long =
  def getUnexpandedDistance(s: Int, t: Int): Int = // Manhattan distance in one dimension
    (t - s).abs
  def getNongalaxyItemCount(s: Int, t: Int, searchSpace: Vector[Int]): Int = // number of non-galaxy items on path
    val potentialGalaxyCount: Int = List(List(s, t).max - List(s, t).min - 1, 0).max
    val galaxyCount: Int = searchSpace.count(g => g > List(s, t).min && g < List(s, t).max)
    val nonGalaxyCount: Int = potentialGalaxyCount - galaxyCount
    nonGalaxyCount
  def getExpandedDistance(s: Int, t: Int, dimension: Vector[Int]): Long =
    val unexpandedDistance: Int = getUnexpandedDistance(s, t)
    val nongalaxySpace: Long = getNongalaxyItemCount(s, t, dimension) * (expansionFactor - 1)
    val result = unexpandedDistance + nongalaxySpace
    result
  val rowDistance: Long = getExpandedDistance(source.row, target.row, galaxiesToExpand("rows"))
  val colDistance: Long = getExpandedDistance(source.col, target.col, galaxiesToExpand("cols"))
  val result = rowDistance + colDistance
  result

@main def main11(): Unit =
  /* Setup */
  val rawInput: Vector[String] = Using(Source.fromResource("12-11_data.txt")) {_.getLines.toVector}.get
  val galaxyChar: Char = '#'
  /** Part 1 */
  val expanded1: Vector[Vector[Char]] = expandMatrix(rawInput, scale = 2)
  val galaxies1: Vector[(Int, Int)] = findGalaxies(expanded1, target = galaxyChar)
  val galaxyPairs1 = findAllGalaxyPairs(galaxies1)
  // galaxyPairs1.foreach(println)
  val distances1 = galaxyPairs1
    .flatMap((e, f) => f.map(g => manhattan(source = e, target = g)))
  val result1 = distances1.sum
  println(s"Part 1 solution: $result1")
  /** Part 2
   *  Part 1 strategy (not surprisingly) causes a heap overflow with expansion factor of 1_000_000 */
  val expansion_factor: Int = 1_000_000 // expansion means adding 999_999 (not 1_000_000) unexpanded rows or cols
  val explodedInput: Vector[Vector[Char]] = rawInput.map(_.toVector) // convert to vector of vector of chars
  val rowsWithGalaxies: Vector[Int] = findRowsWithGalaxies(explodedInput, galaxyChar)// unexpanded
  val colsWithGalaxies: Vector[Int] = findRowsWithGalaxies(explodedInput.transpose, galaxyChar) // unexpanded
  val searchSpace: Map[String, Vector[Int]] = Map("rows" -> rowsWithGalaxies, "cols" -> colsWithGalaxies)
  val galaxies2: Vector[Galaxy] = findGalaxies2(rawInput.map(_.toVector), galaxyChar)
  val galaxyPairs2: SortedMap[Galaxy, Vector[Galaxy]] = findGalaxyPairs2(galaxies2)
  val galaxiesPaired2 = galaxyPairs2
    .flatMap((s, vt) => vt.map(e => expandManhattan(s, e, expansion_factor, searchSpace)))
  val totalDistance = galaxiesPaired2.sum
  println(s"Part 2 solution: $totalDistance")


//https://stackoverflow.com/questions/19345030/easy-idiomatic-way-to-define-ordering-for-a-simple-case-class
case class Galaxy(row: Int, col: Int) extends Ordered[Galaxy] {
  import scala.math.Ordered.orderingToOrdered
  def compare(that: Galaxy): Int = (this.row, this.col) compare (that.row, that.col)
}
