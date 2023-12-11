package Advent_12_10

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/** 2023-12-10, Part 1
 *
 * Border fill
 *
 * Assume: Border is fully connected with no false forks.
 *
 * 1. Start at starting cell, deducing directions by surveying neighbors.
 * 2. Traverse in any direction, end when return to start, counting cells.
 * 3. Maximally far call is distance / 2.
 *
 * No need to mark cells as checked, since we never visit non-border cells
 * as long as we know the starting point.
 */

/** Map ascii input to Unicode box-drawing characters
 *
 * Improve legibility
 * Default to space (shouldn't happen)
 *
 * @param char ascii character to remap as char
 * @return remapped character as char
 */
private def createBoxChar(char: Char): BoxChar =
  val mapping: Map[Char, BoxChar] =
    List(
      '|' -> BoxChar(orig = '|', render = '┃', directions = "UD"),
      '-' -> BoxChar(orig = '-', render = '━', directions = "LR"),
      'J' -> BoxChar(orig = 'J', render = '┛', directions = "UL"),
      'L' -> BoxChar(orig = 'L', render = '┗', directions = "UR"),
      '7' -> BoxChar(orig = '7', render = '┓', directions = "DL"),
      'F' -> BoxChar(orig = 'F', render = '┏', directions = "DR"),
      'S' -> BoxChar(orig = 'S', render = 'S', directions = "XX")
    ).toMap
  mapping(char)

/** Map characters in input row to cells only if actual cell
 *
 * @param rowChars all characters in row as String
 * @param rowNo    row number as Int
 * @return Vector[Cell] with row, column, and data for all data cells in row
 *         filtering out non-data cells
 */
private def createCellsForRow(rowChars: String, rowNo: Int): Vector[Cell] =
  rowChars.zipWithIndex // add column numbers
    .filter((char, colNo) => "|-JL7FS".contains(char)) // keep only cells that might be part of path
    .map((inputChar, colNo) => Cell(row = rowNo, col = colNo, contents = createBoxChar(inputChar)))
    .toVector

/** Create map from ascii character in input to Cell object
 *
 * @param cells Vector[Cell] for all cells in corpus
 * @return Map[row: Int, col: Int), Cell]
 */
private def buildCellMap(cells: Vector[Cell]): Map[(Int, Int), Option[Cell]] =
  cells.map(cell => (cell.row, cell.col) -> Some(cell)).toMap

/** Find neighbors of start cell by brute force
 *
 * @param startCell start cell as Cell
 * @param cellMap   Map([row: Int, col: Int), Cell] to look up cells by position in matrix
 * @return all (both) connected neighbors of start cell as Vector[Cell]
 */
private def findStartNeighbors(startCell: Cell, cellMap: Map[(Int, Int), Option[Cell]]) =
  val result =
    Vector(
      cellMap.getOrElse((startCell.row + 1, startCell.col),None),
      cellMap.getOrElse((startCell.row - 1, startCell.col), None),
      cellMap.getOrElse((startCell.row, startCell.col + 1), None),
      cellMap.getOrElse((startCell.row, startCell.col - 1), None)
    ).filter(_.isDefined)
  println(s"start neighbors = $result")
  result

/** Find next neighboring cell
 *
 * Assumes border is fully connected with no forking, so:
 * All border cells have two connections that are within the space
 * (no need to check of overflow of space)
 * Choose the one we didn't come in on
 *
 * @param focus   cell whose neighbors are sought
 * @param cellMap map from (row,col) to Cell instance
 * @return neighboring cell for next step (filtering out incoming neighbor)
 */
private def findAllCells(
                          focus: Cell,
                          cellMap: Map[(Int, Int), Option[Cell]],
                          startCell: Cell
                        ): Set[Cell] =
  @tailrec
  def findNextCell(focus: Cell, tracker: Set[Cell]): Set[Cell] =
    val inDirections = focus.contents.directions
    val neighbors = inDirections.map {
      case 'U' => cellMap.getOrElse((focus.row - 1, focus.col), None)
      case 'D' => cellMap.getOrElse((focus.row + 1, focus.col), None)
      case 'L' => cellMap.getOrElse((focus.row, focus.col - 1), None)
      case 'R' => cellMap.getOrElse((focus.row, focus.col + 1), None)
    }.map(e => e.orNull)
    val result = neighbors.filterNot(e => tracker.contains(e))
    result match {
      case e if e.isEmpty => tracker // exit condition
      case e => findNextCell(focus = result.head, tracker = tracker + result.head)
    }

  findNextCell(focus = focus, tracker = Set[Cell](startCell, focus))

@main def main10(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-10_data.txt").getLines.toVector
  val allCells = rawInput
    .zipWithIndex
    .flatMap(createCellsForRow)
  val cellMap: Map[(Int, Int), Option[Cell]] = buildCellMap(allCells)
  val startCell: Cell = allCells.filter(_.contents.orig == 'S').head
  println(s"start cell = $startCell")
  val startNeighbor = findStartNeighbors(startCell, cellMap).head
  val path: Set[Cell] = findAllCells(startNeighbor match {case Some(e) => e}, cellMap, startCell)
  val steps: Int = (path.size + 1) / 2
  println(s"Part 1: max distance from start is $steps steps")


//  println(findNeighbors(focus = testCell, lineLength = lineLength, lineCount = lineCount))

/** BorderChar
 *
 * @param orig       original value as Char (ascii character)
 * @param render     rendering value as Char (Unicode box-drawing character)
 * @param directions two-character string with connected directions
 */
case class BoxChar(orig: Char, render: Char, directions: String)

case class Cell(row: Int, col: Int, contents: BoxChar)