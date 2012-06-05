object Freezer {
  def main(args: Array[String]) {
    for (i <- freeze(0).take(10)) println(i)
    for (i <- freeze("Hello").take(10)) println(i)
  }

  def freeze[A](value: A): Stream[A] = Stream.cons(value, freeze(value))
}
