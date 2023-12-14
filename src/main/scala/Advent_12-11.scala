package Advent_12_11

import scala.annotation.tailrec
import scala.io.Source

/** Expand rows and columns in matrix
 *
 * Empty rows and columns are duplicated to expand matrix
 * Change type from Vector[String] to Vector[Vector[Char]] for subsequent processing
 *
 * @param in original matrix as Vector[String]
 * @return expanded matrix as Vector[Vector[Char]]
 */
private def expandMatrix(in: Vector[String]): Vector[Vector[Char]] =
  val result: Vector[Vector[Char]] = in
    .map(_.toVector)
    .flatMap(e => expandRows(e, '#')) // = map and flatten
    .transpose
    .flatMap(e => expandRows(e, '#'))
    .transpose // transpose again so that rows and columns are as in input
  result

/** Duplicate a row if empty, otherwise return unchanged
 *
 * @param in one row as Vector[Char]
 * @return the input plus an additional copy if the row contains no #
 */
private def expandRows(in: Vector[Char], target: Char): Vector[Vector[Char]] =
  in match
    case e if e.contains(target) => Vector(e)
    case e => Vector(e, e)

/** Find all galaxies (# characters) in matrix and return (row, col)
 *
 * @param in matrix as Vector[Vector[Char])
 * @return all locations of galaxies as (row: Int, col: Int)
 */
private def findGalaxies(in: Vector[Vector[Char]]): Vector[(Int, Int)] =
  val result: Vector[(Int, Int)] = in
    .zipWithIndex
    .flatMap(e => {
      val rowNo: Int = e._2
      val indexes: Vector[Int] = findAllIndexes(e._1, '#')
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
  result

/** Find all pairs of galaxies in order to compute pairwise manhattan distances
 *
 * @param galaxies locations of galaxies ('#' characters) as Vector[(row: Int, col: Int)]
 * @return all pairwise combinations of locations except (self, self) as Vector[((Int, Int), (Int, Int))]
 */
private def findAllGalaxyPairs(galaxies: Vector[(Int, Int)]) =
  val result: Vector[((Int, Int), (Int, Int))] = (for {
    source <- galaxies
    target <- galaxies
  } yield (source, target))
    .filter((e, f) => e != f)
  result

@main def main11(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-11_data.txt").getLines.toVector
  val expanded: Vector[Vector[Char]] = expandMatrix(rawInput)
  val galaxies: Vector[(Int, Int)] = findGalaxies(expanded)
  val galaxyPairs: Vector[((Int, Int), (Int, Int))] = findAllGalaxyPairs(galaxies)
  val distances: Vector[Int] = galaxyPairs.map(e => manhattan(e._1, e._2))
  val result1 = distances.sum / 2 // Pairs contain (a,b) and (b, a) so divide by 2 to remove mirrors
  println(s"Part 1 solution: $result1")


