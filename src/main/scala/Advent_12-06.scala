import scala.io.Source

private def parseInput(filename: String): Vector[Race] =
  val rawData = Source.fromResource(filename).mkString("")
  val dataParts = rawData.split("\n").map(_.split("\\s+"))
  val stats = dataParts.head.zip(dataParts.last).tail.map(e => Race(e._1.toInt, e._2.toInt))
  stats.toVector

private def computeTimeDistanceMapping(record: Race): Vector[Race] =
  val mappings =
    for pressTime: Int <- 0 to record.time yield
      val runTime: Int = record.time - pressTime
      Race(pressTime, runTime * pressTime)
  mappings.filter(e => e.distance > record.distance).toVector

@main def main6(): Unit =
  /* ***
  * Setup
  */
  val stats: Vector[Race] = parseInput("12-06_data.txt")
  // val stats: Vector[Race] = parseInput("12-06_data_test.txt")
  val winnerCounts = stats.map(e => computeTimeDistanceMapping(e).length)
  val winnerProduct = winnerCounts.product
  println(s"Part 1: $winnerProduct")

case class Race(time: Int, distance: Int)
