package Advent_12_09

import scala.annotation.tailrec
import scala.io.Source

private def processRow(in: String): (Int, Int) =
  @tailrec
  def findDiffs(nums: Vector[Int], acc: Vector[Vector[Int]]): Vector[Vector[Int]] =
    val nextRow: Vector[Int] = nums.sliding(2).map(e => e(1) - e.head).toVector
    nextRow match
      case e if nextRow.forall(_ == 0) => acc :+ nextRow
      case _ => findDiffs(nums = nextRow, acc = acc :+ nextRow)

  @tailrec
  def findResult1(rows: Vector[Vector[Int]], prediction: Int): Int =
    val newPrediction: Int = prediction + rows.head.last
    rows match
      case e if e.size == 1 => newPrediction // last row, so return
      case _ => findResult1(rows = rows.tail, prediction = prediction + rows.head.last)

  @tailrec
  def findResult2(rows: Vector[Vector[Int]], prediction: Int): Int =
    val newPrediction: Int = rows.head.head - prediction
    rows match
      case e if e.size == 1 => newPrediction
      case _ => findResult2(rows = rows.tail, prediction = newPrediction )

  val init: Vector[Int] = in
    .split("""\s+""")
    .map(_.toInt)
    .toVector
  val allRows: Vector[Vector[Int]] = findDiffs(nums = init, acc = Vector(init))
  val result1: Int = findResult1(allRows.reverse, prediction = 0)
  val result2: Int = findResult2(allRows.reverse, prediction = 0)
  (result1, result2)

@main def main12(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-09_data.txt").getLines.toVector
  val (results1: Int, results2: Int) = rawInput
    .map(processRow)
    .reduce((e: (Int, Int), f: (Int, Int)) => (e._1 + f._1, e._2 + f._2))
  println(s"Part 1: $results1")
  println(s"Part 2: $results2")
