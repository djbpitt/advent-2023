package Advent_12_09

import scala.annotation.tailrec
import scala.io.Source

private def processRow(in: String) =
  @tailrec
  def findDiffs(nums: Vector[Int], acc: Vector[Vector[Int]]): Vector[Vector[Int]] =
    val nextRow: Vector[Int] = nums.sliding(2).map(e => e(1) - e.head).toVector
    println(s"nextRow: $nextRow")
    nextRow match
      case e if nextRow.forall(_ == 0) => acc :+ nextRow
      case _ => findDiffs(nums = nextRow, acc = acc :+ nextRow)

  @tailrec
  def findResult(rows: Vector[Vector[Int]], prediction: Int): Int =
    val newPrediction: Int = prediction + rows.head.last
    println(s"newPrediction: $newPrediction")
    rows match
      case e if e.size == 1 => newPrediction // last row, so return
      case _ => findResult(rows = rows.tail, prediction = prediction + rows.head.last)

  val init: Vector[Int] = in
    .split("""\s+""")
    .map(_.toInt)
    .toVector
  val allRows: Vector[Vector[Int]] = findDiffs(nums = init, acc = Vector(init))
  println(s"allRows: $allRows")
  val result = findResult(allRows.reverse, prediction = 0)
  result

@main def main12(): Unit =
  val rawInput: Iterator[String] = Source.fromResource("12-09_data_test.txt").getLines
  val result = processRow(in = rawInput.next())
  println(s"result: $result")