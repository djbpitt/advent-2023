import scala.io.Source

private def parseInput(filename: String): Vector[Race] =
  val rawData = Source.fromResource(filename).mkString("")
  val dataParts = rawData.split("\n").map(_.split("\\s+"))
  val stats = dataParts.head.zip(dataParts.last).tail.map(e => Race(e._1.toInt, e._2.toInt))
  stats.toVector

@main def main6(): Unit =
  /* ***
  * Setup
  */
  // val stats = parseInput("12-06_data.txt")
  val stats = parseInput("12-06_data_test.txt")
  println(stats)

case class Race(time: Int, distance: Int)