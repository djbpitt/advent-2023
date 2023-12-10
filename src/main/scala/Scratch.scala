package scratch

/*
| is a vertical pipe connecting north and south. 2503
- is a horizontal pipe connecting east and west. 2501
L is a 90-degree bend connecting north and east. 2517
J is a 90-degree bend connecting north and west. 251B
7 is a 90-degree bend connecting south and west. 2513
F is a 90-degree bend connecting south and east. 250F
 */

val sample: String = """.....
                       |.F-7.
                       |.|.|.
                       |.L-J.
                       |.....""".stripMargin

private def mapToBoxChar(char: Char): Char =
  val mapping: Map[Char, Char] =
    List('|' -> '┃',
    '-' -> '━',
    'L' -> '┗',
    'J' -> '┛',
    '7' -> '┓',
    'F' -> '┏',
    '.' -> '·',
    '\n' -> '\n').toMap
  mapping.getOrElse(char, ' ')


@main def scratch(): Unit =
  println(sample.map(mapToBoxChar))
  println(sample)