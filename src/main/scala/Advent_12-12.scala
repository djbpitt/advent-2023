package Advent_12_12

import scala.util.Using
import scala.io.Source
import scala.util.matching.Regex

@main def main12(): Unit =
  /* Setup */
  val rawInput: Vector[String] = Using(Source.fromResource("12-12_data.txt")) {_.getLines.toVector}.get
  val p1 = rawInput.map(_.split(" ").toVector)
  println(p1)
  val p2 = p1.map(e => {
    val data = e.head
    val groupSizes = e.last.split(",").toVector
    Vector(data, groupSizes)
  })
  println(p2)
  val dataSizes = p2.map(_.head)
  dataSizes.foreach(println)
  /** Part 1 */
