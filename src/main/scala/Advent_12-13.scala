package Advent_12_13

import scala.annotation.tailrec
import scala.util.Using
import scala.io.Source
import scala.util.matching.Regex

// https://hackernoon.com/manachers-algorithm-explained-longest-palindromic-substring-22cb27a5e96f
@main def main13(): Unit =
  /* Setup */
  val rawLines = Source.fromResource("12-13_data_test.txt")
    .getLines()
    .mkString("\n")
  val tasks = rawLines.split("\n\n").toVector.map(_.split("\n").map(_.toVector).toVector)








