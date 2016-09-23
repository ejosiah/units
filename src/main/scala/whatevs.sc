val list = List(1, 2, 3)

list match {
  case l @ head :: tail =>
    println(head)
    println(tail)
    println(l)
    println(list)
}