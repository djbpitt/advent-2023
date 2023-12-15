package Advent_12_11

import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.io.Source

/** Expand rows and columns in matrix
 *
 * Empty rows and columns are duplicated to expand matrix
 * Change type from Vector[String] to Vector[Vector[Char]] for subsequent processing
 * TODO: Target character doesn't need to be specified, since the any non-dot must be a galaxy
 *
 * @param in     original matrix as Vector[String]
 * @param target character whose presence prevents expansion as Char
 * @param scale  number of copies to make if target not found as Int
 * @return expanded matrix as Vector[Vector[Char]]
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
 *
 * @param in     one row as Vector[Char]
 * @param target character that signals galaxy and prevents expansion as Char
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
private def findAllGalaxyPairs(galaxies: Vector[(Int, Int)]): Vector[((Int, Int), (Int, Int))] =
  val result = for {
    source <- galaxies
    target <- galaxies
    if source._1 <= target._1 && target != source
  } yield (source, target)
  result

@main def main11(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-11_data_test.txt").getLines.toVector
  val expanded1: Vector[Vector[Char]] = expandMatrix(rawInput, scale = 2)
  val galaxies1: Vector[(Int, Int)] = findGalaxies(expanded1)
  val galaxyPairs1: Map[(Int, Int), Vector[((Int, Int), (Int, Int))]] =
    findAllGalaxyPairs(galaxies1).groupBy((e, f) => e)
  val galaxyPairs1sorted: Seq[((Int, Int), Vector[(Int, Int)])] =
    for {k <- galaxyPairs1.keys.toSeq.sorted} yield
      k -> galaxyPairs1(k).map(_._2).sorted
  val galaxyPairsMapping: SortedMap[(Int, Int), Vector[(Int, Int)]] = SortedMap(galaxyPairs1sorted*)
  galaxyPairsMapping.foreach(println)
//  val distances1: Vector[Int] = galaxyPairs1.map(e => manhattan(e._1, e._2))
//  val result1 = distances1.sum
  // Part 2 expands by 1_000_000
  // Currently set for 2; heap overflow with 1_000_000

//  println(s"Part 1 solution: $result1")



