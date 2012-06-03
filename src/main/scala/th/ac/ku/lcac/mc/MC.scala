package th.ac.ku.lcac.mc

/**
 * Monte Carlo Simulation
 */
trait MC[A] {

  def accept(o: A, n: A): Boolean

  def move(a: A): A

}

object MC {

  def compose[A](first: A)(implicit mc: MC[A]): Stream[A] = {

    def move(o: A): Stream[A] = {
      val n = mc.move(o)

      if (mc.accept(o, n))
        Stream.cons(n, move(n))
      else
        move(o)
    }

    Stream.cons(first, move(first))
  }

}
