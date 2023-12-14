import scala.annotation.tailrec
import scala.io.Source

private def expand(in: String): Vector[String] =
  in match
    case e if e.contains('#') => Vector(e)
    case e => Vector(e, e)

private def transpose(in: Vector[String]): Vector[String] =
  val lineCount = in.size
  val lineLength = in.head.length
  val transposed =
    (for i <- 0 until lineLength yield
      (for j <- 0 until lineCount yield
        (in(j)(i))).mkString).toVector
  transposed

@main def main11(): Unit =
  val rawInput: Vector[String] = Source.fromResource("12-11_data_test.txt").getLines.toVector
  val expandedRows = rawInput.map(expand).flatten
  println("Expanded rows:")
  println(expandedRows.mkString("\n"))
  val transposed = transpose(expandedRows)
  println("Transposed:")
  println(transposed.mkString("\n"))
  val expandedCols = transposed.map(expand).flatten
  println("Expanded but still transposed:")
  println(expandedCols.mkString("\n"))
  val fullyExpanded = transpose(expandedCols)
  println("Fully expanded and reoriented:")
  println(fullyExpanded.mkString("\n"))

