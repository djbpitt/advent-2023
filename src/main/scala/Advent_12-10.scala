package Advent_12_10

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

private def createCellsForRow(rowChars: String, rowNo: Int): Vector[Cell] =
  rowChars.zipWithIndex // add column numbers
    .filter((char, colNo) => "|-JL7FS".contains(char)) // keep only cells that might be part of path
    .map((inputChar, colNo) => Cell(row = rowNo, col = colNo, contents = createBoxChar(inputChar)))
    .toVector

private def buildCellMap(cells: Vector[Cell]): Map[(Int, Int), Option[Cell]] =
  cells.map(cell => (cell.row, cell.col) -> Some(cell)).toMap

private def findStartNeighbors(startCell: Cell, cellMap: Map[(Int, Int), Option[Cell]]) =
  val result: Vector[Cell] =
    Vector(
      (startCell.row, startCell.col - 1),
      (startCell.row, startCell.col + 1),
      (startCell.row - 1, startCell.col),
      (startCell.row + 1, startCell.col)
    )
      .map(e => cellMap.getOrElse(e, None) match {
        case Some(e) => e
        case e => println(s"didn't match $e"); startCell
      }).filterNot(_ equals startCell)
  println(s"start neighbors = $result")
  result


/** Find neighboring cells
 *
 * Assumes border is fully connected with no forking, so:
 * All border cells have two connections that are within the space
 * (no need to check of overflow of space)
 *
 * Elsewhere: choose the one we didn't come in on
 *
 * @param focus cell whose neighbors are sought
 * @param cellMap map from (row,col) to Cell instance
 * @return
 */
private def findNextCell(focus: Cell, cellMap: Map[(Int, Int), Cell]): Cell =
  println(s"focus: $focus")
  val inDirections = focus.contents.directions
  val neighbors = inDirections.map(e =>
    println(e)
    e match {
      case 'U' => cellMap.getOrElse((focus.row - 1, focus.col), None)
      case 'D' => cellMap.getOrElse((focus.row + 1, focus.col), None)
      case 'L' => cellMap.getOrElse((focus.row, focus.col - 1), None)
      case 'R' => cellMap.getOrElse((focus.row, focus.col + 1), None)
    })
  println(s"neighbors = $neighbors")
  focus


@main def main10(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-10_data_test_1.txt").getLines.toVector
  val allCells = rawInput
    .zipWithIndex
    .flatMap(createCellsForRow)
  val cellMap: Map[(Int, Int), Option[Cell]] = buildCellMap(allCells)
  val startCell: Cell = allCells.filter(_.contents.orig == 'S').head
  val startNeighbors = findStartNeighbors(startCell, cellMap)
  println(s"startCell = $startCell")
  println(s"startNeighbors = $startNeighbors")
//  val nextCell: Cell = findNextCell(startNeighbors.head, cellMap)
//  println(s"nextCell = $nextCell")


//  println(findNeighbors(focus = testCell, lineLength = lineLength, lineCount = lineCount))

/** BorderChar
 *
 * @param orig       original value as Char (ascii character)
 * @param render     rendering value as Char (Unicode box-drawing character)
 * @param directions two-character string with connected directions
 */
case class BoxChar(orig: Char, render: Char, directions: String)

case class Cell(row: Int, col: Int, contents: BoxChar)