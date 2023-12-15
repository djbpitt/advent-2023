package scratch



@main def scratch(): Unit =
  val v: Vector[Int] = Vector(1, 2, 3, 4)
  val result = for {
    r <- v
    c <- v
    if r < c
  } yield (r, c)
  println(result)