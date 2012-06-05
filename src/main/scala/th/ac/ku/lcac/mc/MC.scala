package th.ac.ku.lcac.mc

/**
 * Monte Carlo Simulation
 */
trait MC[A] {

  def accept(o: A, n: A): Boolean

  def move(a: A): A

}

object MC {

  def compose[A: MC](first: A): Stream[A] = {

    val mc = implicitly[MC[A]]

    def move(o: A): Stream[A] = {
      val n = mc.move(o)

      val t = if (mc.accept(o, n)) n else o
      Stream.cons(t, move(t))
    }

    Stream.cons(first, move(first))
  }

}
