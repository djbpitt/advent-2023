import scala.io.Source
import scala.util.matching.Regex

val nonDigitpattern: Regex = """\D+""".r
private def parseInput_1(filename: String): Vector[Race] =
  val rawData = Source.fromResource(filename).mkString("")
  val dataParts = rawData.split("\n").map(_.split("\\s+"))
  val stats = dataParts.head.zip(dataParts.last).tail.map(e => Race(e._1.toLong, e._2.toLong))
  stats.toVector

private def computeTimeDistanceMapping(record: Race): Vector[Race] =
  val mappings =
    for pressTime: Long <- 0L to record.time yield
      val runTime: Long = record.time - pressTime
      Race(pressTime, runTime * pressTime)
  mappings.filter(e => e.distance > record.distance).toVector


private def parseInput_2(filename: String): Race =
  val rawData: String = Source.fromResource(filename).mkString("")
  val dataParts = rawData.split("\n")
  val time: Long = nonDigitpattern.replaceAllIn(dataParts.head, "").toLong
  val distance: Long = nonDigitpattern.replaceAllIn(dataParts.last, "").toLong
  Race(time, distance)


def main6_1(): Unit =
  val stats: Vector[Race] = parseInput_1("12-06_data.txt")
  // val stats: Vector[Race] = parseInput_1("12-06_data_test.txt")
  val winnerCounts = stats.map(e => computeTimeDistanceMapping(e).length)
  val winnerProduct = winnerCounts.product
  println(s"Part 1: $winnerProduct")

def main6_2(): Unit =
  val stats: Race = parseInput_2("12-06_data.txt")
  println(stats)

@main def main6(): Unit =
  main6_1()
  main6_2()

case class Race(time: Long, distance: Long)
