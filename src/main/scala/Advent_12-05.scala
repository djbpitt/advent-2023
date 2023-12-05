import os.*

/** parseSeeds
 *
 * Input is string that begins with label. Discard label and split
 * string into longs
 *
 * @param seeds string
 * @return vector of longs
 */
private def parseSeeds(seeds: String): Vector[Long] =
  seeds
    .split(": ") // separate and throw away label
    .last
    .split("""\s""") // split values on whitespace
    .map(_.toLong) // convert strings to longs
    .toVector

/** parseSection
 *
 * Throw away label (for now); sections are in feeding order, so they
 * don’t need to find one another
 * Split remainder into strings consisting of three space-separated numerical values
 * Split each of those strings into three longs and create Mapping object
 *
 * @param section multiline string
 * @return vector of Mapping objects, each with destinationStart, sourceStart,
 *         and length
 */
private def parseSection(section: String): Vector[Mapping] =
  val lines = section.split("\n").map(_.trim)
  // label parts are information; sections are ordered, so labels aren’t needed
  // val label = lines.head.takeWhile(_ != ' ')
  // val labelParts = label.split("-")
  // val labelFrom = labelParts(2)
  // val labelTo = labelParts.head
  val body = lines.tail.toVector
  val bodyParts =
    for mapping <- body yield
      val mappingParts = mapping.split(" ").map(_.toLong)
      Mapping(mappingParts.head, mappingParts(1), mappingParts(2))
  bodyParts

/** mapToSection
 *
 * Filter all Mapping objects in section to keep one (or none) that contain seed
 * If applicable Mapping found, remap value
 * Otherwise return value unchanged
 *
 * Partially applied for easier composition
 *
 * @param section vector of Mapping objects describing mapped ranges
 * @param seed    long, which appears in zero or one range
 * @return long, remapped if in range and unchanged if not
 */
private def mapToSection(section: Vector[Mapping])(seed: Long) =
  section.filter(e => seed >= e.sourceStart && seed < e.sourceStart + e.length) match {
    case f if f.nonEmpty => seed - f.head.sourceStart + f.head.destinationStart
    case _ => seed
  }

def main5_1(): Unit =
  /* ***
  * Setup
  */
  val data = os.read(os.pwd / "src" / "resources" / "12-05_data_test.txt")
  // val data: String = os.read(os.pwd / "src" / "resources" / "12-05_data.txt")
  /* ***
   * Break out vectors of seeds and sections
   * Map sections to partially applied functions that require only seed or other long
   * Compose functions into chain by reducing across andThen
   */
  val sectionStrings: Vector[String] = data.split("\n\n").toVector
  val seeds = parseSeeds(sectionStrings.head)
  val sections = sectionStrings.tail.map(parseSection) // vector of Mapping objects (from, to, length)
  val sectionFunctions = sections.map(mapToSection) // arity-1 functions make for easier composition
  // https://users.scala-lang.org/t/best-practice-applying-multiple-transformations-using-fold/7262/2
  val composed = sectionFunctions.reduce(_ andThen _)
  val results = seeds.map(composed)
  println(results)

@main def main5(): Unit =
  main5_1()

case class Mapping(destinationStart: Long, sourceStart: Long, length: Long)
