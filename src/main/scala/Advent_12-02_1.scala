import os.*

import scala.util.matching.Regex
// https://stackoverflow.com/questions/2066170/scala-regexps-how-to-return-matches-as-array-or-list
/** All games have at least one of each color
 * Games are sequential, so could use offset instead of embedded game number
 *
 * GameData has an integer game number and a struct of (min,max) integers for each color
 * */
case class ColorData(min:Int, max:Int)
case class GameData(game: Int, red: ColorData, blue: ColorData, green: ColorData)

val gameNoPattern: Regex = "Game (\\d+)".r
val redCountPattern: Regex = "(\\d+) red".r
val greenCountPattern: Regex = "(\\d+) green".r
val blueCountPattern: Regex = "(\\d+) blue".r

/** Parse game data and create GameData object for each game
 *
 * Regex matching creates an iterator; convert to set to avoid depleting it during access
 * */
def countColors(in: String): GameData =
  val gameNo = gameNoPattern.findFirstMatchIn(in).get.subgroups.head.toInt
  val reds =
    (for (m <- redCountPattern.findAllIn(in).matchData; e <- m.subgroups) yield e.toInt).toSet
  val greens =
    (for (m <- greenCountPattern.findAllIn(in).matchData; e <- m.subgroups) yield e.toInt).toSet
  val blues =
    (for (m <- blueCountPattern.findAllIn(in).matchData; e <- m.subgroups) yield e.toInt).toSet
  GameData(
    game = gameNo,
    red = ColorData(min = reds.min, max = reds.max),
    blue = ColorData(min = blues.min, max = blues.max),
    green = ColorData(min = greens.min, max = greens.max)
  )
@main def main2_1(): Unit =
  val data_lines = os.read(os.pwd/"src"/"resources"/"12-02_data.txt")
    .split("\n")
  val game_data = data_lines.map(e => countColors(e))
  val playable = game_data.filter(e => e.red.max <= 12 && e.blue.max <= 14 && e.green.max <= 13)
  println(playable.map(_.game).sum)




