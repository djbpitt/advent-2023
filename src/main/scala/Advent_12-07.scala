object Cards extends Enumeration {
  type Card = Value
  val C2, C3, C4, C5, C6, C7, C8, C9, CT, CJ, CQ, CK, CA = Value
}
import Cards._

@main def t(): Unit =
  val all = Vector(C7, CK, C3, CA, CT)
  println(all.min)
  println(all.max)