import scala.annotation.tailrec
import scala.io.Source

private def parsePointer(direction: String) =
  direction match {
    case s"${key} = (${left}, ${right})" =>
      key -> Map('L' -> left, 'R' -> right)
  }
private def readData(filename: String) =
  val rawData = Source.fromResource(filename).getLines().toVector
  val directions = rawData.head
  val mapItems = rawData.drop(2)
  (directions, mapItems)

private def findZZZ(directions: Vector[Char], pointers: Map[String, Map[Char, String]]) =
  val directionsLength: Int = directions.length
  @tailrec
  def takeStep(stepNo: Int, stepStart: String, direction: Char): Int =
    val nextStepStart = pointers(stepStart)(direction)
    if nextStepStart == "ZZZ" || stepNo == 10 then
      stepNo
    else
      val nextStepNo = stepNo + 1
      val nextDirection = directions(stepNo % directionsLength)
      takeStep(nextStepNo, nextStepStart, nextDirection)
  takeStep(1, "AAA", directions.head)

@main def main8(): Unit =
  val rawData = readData("12-08_data_test_2.txt")
  val directions: Vector[Char] = rawData.head.toVector // LR …
  val pointerData: Vector[String] = rawData.last
  val pointers = pointerData.map(parsePointer).toMap
  val result = findZZZ(directions, pointers)
  println(result)

