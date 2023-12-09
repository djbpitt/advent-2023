package scratch

object Hands extends Enumeration {
  type Card = Value
  val High, Mid, Low = Value
}

import Hands._
protected case class Hands(i: Int, name: String, promote: Value)
  def promote(in: Value): Value =
    val promoted = Hands(in.id + 1)
    promoted

@main def scratch(): Unit =
  val promoted = promote(Mid)
  println(promoted)