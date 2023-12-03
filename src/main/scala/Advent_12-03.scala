import os.*

def identifyChar(haystack: Array[String], row: Int, needle: Char): Map[(Int, Int), Char] =
  val map_entries = haystack(row)
    .zipWithIndex
    .map(e => (row, e._2) -> e._1)
  map_entries.toMap

@main def main3(): Unit =
  // Setup; data is 140 x 140
  val data_lines = os.read(os.pwd / "src" / "resources" / "12-03_data.txt")
    .split("\n")
  // Coordinates in data are (row, column), ranging from 0–139
  val symbol_map = identifyChar(data_lines, 0, 'a')
  println(symbol_map)


