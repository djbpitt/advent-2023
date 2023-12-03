import os.*

import scala.annotation.tailrec
import scala.util.matching.Regex

private def identifyChars(haystack: Array[String]): Vector[DataPoint] =
  val dataPoints = haystack
    .zipWithIndex
    .flatMap((rowData, rowIndex) => rowData.zipWithIndex.map(e => (rowIndex, e._2, e._1)))
    .map((row, col, char) => DataPoint(row, col, char))
  dataPoints.toVector

/** identifySymbols
 *
 * Filter all dataPoints to keep only those that contain symbols
 * Remove character for easier comparison to number neighbors
 *
 * @param dataPoints vector of data points
 * @return filtered dataPoints
 */
private def identifySymbols(dataPoints: Vector[DataPoint]): Set[DataPointCoordinates] =
  val symbolPattern: Regex = """[^.\d]""".r
  val symbolPoints = dataPoints.filter(e => symbolPattern
    .matches(e.character.toString))
    .map(f => DataPointCoordinates(f.row, f.col))
  symbolPoints.toSet

/** identifyChars
 *
 * Create DataPoint object with row, column, and character for each point in 140 x 140 matrix
 *
 * @ param haystack Array[String] with 140 rows, each with 140 characters
 * */

/** identifyPlanNumbers
 *
 * @param haystack all data as array of strings, one string per row
 * @return vector of PlanNumber objects with coordinates and value
 */
private def identifyPlanNumbers(haystack: Array[String]): Vector[PlanNumber] =
  val numberPattern: Regex = """\d+""".r

  @tailrec
  def numbersInRow(currentRowNo: Int, result: Vector[PlanNumber]): Vector[PlanNumber] =
    if currentRowNo == haystack.length then
      result
    else
      val newPlanDigits = numberPattern
        .findAllMatchIn(haystack(currentRowNo))
        .map(e => PlanNumber(currentRowNo, e.start, e.end, e.matched.toInt))
      numbersInRow(currentRowNo + 1, result :++ newPlanDigits.toVector)

  val cumulative = numbersInRow(0, result = Vector[PlanNumber]())
  cumulative

private def identifyPlanNumberNeighbors(allPlanNumbers: Vector[PlanNumber]) =
  val planNumberNeighbors =
    allPlanNumbers.map(d =>
      val topNeighbors = (d.colStart - 1 to d.colEnd)
        .map(e =>
          DataPointCoordinates(row = d.row - 1, col = e)
        ).toVector
      val bottomNeighbors = (d.colStart - 1 to d.colEnd)
        .map(e => DataPointCoordinates(row = d.row + 1, col = e)
        ).toVector
      val leftNeighbor = Vector(DataPointCoordinates(row = d.row, col = d.colStart - 1))
      val rightNeighbor = Vector(DataPointCoordinates(row = d.row, col = d.colEnd))
      val allNeighbors = topNeighbors :++ bottomNeighbors :++ leftNeighbor :++ rightNeighbor
      PlanNumberNeighborSet(d, allNeighbors.toSet)
    )
  planNumberNeighbors

@main def main3(): Unit =
  // Setup; data is 140 x 140
  val data_lines = os.read(os.pwd / "src" / "resources" / "12-03_data.txt")
    .split("\n")
  // Coordinates in data are (row, column), ranging from 0–139
  val dataPoints = identifyChars(data_lines) // (row, col, char)
  val symbolPoints: Set[DataPointCoordinates] = identifySymbols(dataPoints)
  val numberPoints: Vector[PlanNumber] = identifyPlanNumbers(data_lines)
  val numberNeighbors: Vector[PlanNumberNeighborSet] = identifyPlanNumberNeighbors(numberPoints)
  val numbersWithSymbolNeighbors =
    numberNeighbors.filter(e => (e.numberNeighborSet intersect symbolPoints).nonEmpty)
  val numberValues = numbersWithSymbolNeighbors
    .map(e => e.numberInstance.value)
  println(numberValues.sum)

case class DataPoint(row: Int, col: Int, character: Char)
case class DataPointCoordinates(row: Int, col:Int)

/** Number
 *
 * @param row      : Int
 * @param colStart : Int
 * @param colEnd   : Int (exclusive)
 * @param value    : Int
 */
case class PlanNumber(row: Int, colStart: Int, colEnd: Int, value: Int)

case class PlanNumberNeighborSet(numberInstance: PlanNumber, numberNeighborSet: Set[DataPointCoordinates])




