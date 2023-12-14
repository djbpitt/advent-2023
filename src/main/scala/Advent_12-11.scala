package Advent_12_11

import scala.annotation.tailrec
import scala.io.Source

private def expandMatrix(in: Vector[String]): String =
  val result: String = in
    .map(_.toVector)
    .flatMap(expandRows) // = map and flatten
    .transpose
    .flatMap(expandRows)
    .transpose // transpose again so that rows and columns are as in input
    .map(_.mkString) // stringify rows
    .mkString("\n") // stringify matrix
  result

private def expandRows(in: Vector[Char]): Vector[Vector[Char]] =
  in match
    case e if e.contains('#') => Vector(e)
    case e => Vector(e, e)

@main def main11(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-11_data_test.txt").getLines.toVector
  val expanded: String = expandMatrix(rawInput)


