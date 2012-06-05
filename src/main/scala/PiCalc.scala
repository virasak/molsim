package th.ac.ku.lcac.molsim

object PiCalc {

  val random = new scala.util.Random()

  def main(args: Array[String]) {
    val iteration = 100000
    println("Pi calculation by using MC for " + iteration + " iterations:")
    println(piWithMC(iteration))
  }

  def piWithMC(total: Int): Double = {
    val cc = dropInUnitCircle().take(total).filter(_==true).size
    // area of circle/area of square = Pi/4 == cc / total, total -> Inf
    4.0 * cc / total
  }

  def dropInUnitCircle(): Stream[Boolean] = {
    val x = random.nextDouble()
    val y = random.nextDouble()
    val r = math.sqrt(x*x + y*y)

    Stream.cons(r <= 1.0, dropInUnitCircle())
  }
}
