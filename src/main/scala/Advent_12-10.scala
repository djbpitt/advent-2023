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
  val neighbors: Vector[Cell] =
    Vector((startCell.row + 1, startCell.col), // coordinates of neighbors of start, in UDLR order
      (startCell.row - 1, startCell.col),
      (startCell.row, startCell.col + 1),
      (startCell.row, startCell.col - 1))
      .map(e => cellMap.get(e)) // get() returns Option, just parens will raise error if not found
      .zip(Vector('U', 'D', 'L', 'R')) // match with values before filtering
      .filter((e, f) => e.isDefined) // exclude none values in case start cell is on edge
      .map((e: Option[Option[Cell]], f: Char) => (e.flatten, f)) // remove stupid outer Some() wrapper
      .map { // remove inner Some() wrapper; guaranteed to match because we filtered out nulls already
        case (Some(e), f) => (e, f)
      }
      .filter((e, f) => e.connectDirections.contains(f)) // does neighbor connect to startCell?
      .map((e, f) => e) // remove Char
  neighbors


/** Find all border cells recursively
 *
 * Assumes border is fully connected with no forking, so:
 * All border cells have two connectDirections that are within the space
 * (no need to check of overflow of space)
 * Choose the one we didn't come in on
 *
 * @param focus   cell whose neighbors are sought
 * @param cellMap map from (row,col) to Cell instance
 * @return neighboring cell for next step (filtering out incoming neighbor)
 */
private def findAllCells(focus: Cell, cellMap: Map[(Int, Int), Option[Cell]], startCell: Cell): Set[Cell] =
  @tailrec
  def findNextCell(focus: Cell, tracker: Set[Cell]): Set[Cell] =
    val inDirections: String = focus.connectDirections
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

/** Print part 1 result as character art
 *
 * @param steps     border cells as Set[Cell]
 * @param rowCount  number of rows as Int
 * @param rowLength number of cols as Int
 * @return border plot as character art
 */
private def plot_1(steps: Set[Cell], rowCount: Int, rowLength: Int): String =
  /*
    Same width as space and box-drawing characters
    2591 ░ LIGHT SHADE
    2592 ▒ MEDIUM SHADE
    2593 ▓ DARK SHADE
  * */
  def plotRow(rowCells: Set[Cell]): String =
    val colsToCells: Map[Int, Cell] = rowCells.map(e => e.col -> e).toMap
    val rowString = (0 to rowLength)
      .map(e => colsToCells.getOrElse(e, None)
      match {
        case e: Cell => e.contents.render
        case _ => " "
      }).mkString
    rowString

  val rows = steps.groupBy(e => e.row)
  (0 to rowCount).map {
    case e if rows.keySet.contains(e) => plotRow(rows(e))
    case _ => " " * rowLength
  }.mkString("\n")

/** Distinguish interior and exterior points
 *
 * Unplaced cells are either even (path from cell to edge crosses an
 * even number of borders cells, including zero) or odd
 *
 * @param border all border cells as Set[Cell]
 * @return interior cells coordinates as (row: Int, col:Int)
 */
private def findInterior(border: Set[Cell], rowCount: Int, rowLength: Int): Set[(Int, Int)] =
  val borderCellsByRow = border
    .groupBy(_.row)
    .toSeq
    .sortBy(_._1)
    .map(e =>
      val cols = e._2.map(_.col).toSeq.sorted
      e._1 -> cols
    ).toMap

  @tailrec
  def processRowCell(rowNo: Int, colNo: Int, acc: Set[(Int, Int)]): Set[(Int, Int)] =
    colNo match {
      case e if e == rowLength => acc // no more rows, so return
      case e if !borderCellsByRow.keySet.contains(rowNo) => // empty row, so all cells outside
        processRowCell(rowNo, colNo + 1, acc)
      case e => // non-empty row, so count border cells between focus and edge
        (0 to e)
          .intersect(borderCellsByRow(rowNo).filter(_ <= e))
          .size match {
          case g if g % 2 == 0 => processRowCell(rowNo, colNo + 1, acc) // even, so outside
          case g => processRowCell(rowNo, colNo + 1, acc + ((rowNo, colNo)))
        }
    }

  @tailrec
  def processRow(rowNo: Int, acc: Set[(Int, Int)]): Set[(Int, Int)] =
    rowNo match {
      case e if e == rowCount => acc
      case _ =>
        val cellsFromRow = processRowCell(rowNo = rowNo, colNo = 0, acc = acc)
        processRow(rowNo + 1, acc union cellsFromRow)
    }

  val borderCoordinates: Set[(Int, Int)] = border
    .filterNot(e => e.contents.render == '━') // doesn't change parity
    .map(e => (e.row, e.col))
  val oddCells = processRow(0, Set[(Int, Int)]())
    .filterNot(e => borderCoordinates.contains(e))
  // List(borderCellsByRow, borderCoordinates, oddCells).foreach(println)
  oddCells

@main def main10(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-10_data.txt").getLines.toVector
  val allCells = rawInput
    .zipWithIndex
    .flatMap(createCellsForRow)
  val cellMap: Map[(Int, Int), Option[Cell]] = buildCellMap(allCells)
  val startCell: Cell = allCells.filter(_.contents.orig == 'S').head
  val startNeighbors = findStartNeighbors(startCell, cellMap) // should always return exact two Cells
  val borderCells: Set[Cell] = findAllCells(startNeighbors.head, cellMap, startCell)
  val steps: Int = (borderCells.size + 1) / 2
  println(s"Part 1: max distance from start is $steps steps")
  val rowCount: Int = rawInput.size
  val rowLength: Int = rawInput.head.length
  val part1Plot: String = plot_1(borderCells, rowCount, rowLength)
  println(part1Plot)

//  val interiorCells: Set[(Int, Int)] = findInterior(borderCells, rowCount, rowLength)
//  println(s"start neighbors: $startNeighbors")
//  println(interiorCells.toSeq.sorted)

/** BorderChar
 *
 * @param orig       original value as Char (ascii character)
 * @param render     rendering value as Char (Unicode box-drawing character)
 * @param directions two-character string with connected directions (UDLF)
 */
case class BoxChar(orig: Char, render: Char, directions: String)

case class Cell(row: Int, col: Int, contents: BoxChar) {
  def connectDirections: String = contents.directions
}
