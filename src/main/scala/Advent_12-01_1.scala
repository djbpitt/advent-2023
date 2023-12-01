import os._
def main1(): Unit =
  val data = os.read(os.pwd/"src"/"resources"/"12-01_data.txt")
    .split("\n")
    .map(_.filter(_.isDigit))
    .map(e => List(e.head.toString.toInt * 10,  e.last.toString.toInt).sum).toList
  println(data.sum)


