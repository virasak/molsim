package th.ac.ku.lcac.molsim

case class Vector(x: Double, y: Double, z: Double)

trait Coordinates[A] {
  def extract(a: A): Seq[Vector]
  def create(a: A, positions: Seq[Vector]): A
}


object RandomWalker {
  private val random = new scala.util.Random()

  def main(args: Array[String]) {
    val config = Seq((0.0, 0.0, 0.0))

    implicit val xxx = new Coordinates[Seq[(Double,Double,Double)]] {
      def extract(value: Seq[(Double,Double,Double)]) = {
        for (v <- value) yield Vector(v._1, v._2, v._3)
      }

      def create(value: Seq[(Double,Double,Double)], positions: Seq[Vector]) = {
        for (pos <- positions) yield (pos.x, pos.y, pos.z)
      }
    }

    for (tup <- randomWalk(config, 0.5).take(10)) println(tup)
  }

  def randomWalk[A](config: A, maxStepSize: Double)(implicit coords: Coordinates[A]): Stream[A] = {

    def move(config: A): Stream[A] = {

      val positions = for (position <- coords.extract(config)) yield {
        val x = (2.0*random.nextDouble - 1.0) * maxStepSize
        val y = (2.0*random.nextDouble - 1.0) * maxStepSize
        val z = (2.0*random.nextDouble - 1.0) * maxStepSize

        Vector(position.x + x, position.y + y, position.z + z)
      }

      val newConfig = coords.create(config, positions)

      Stream.cons(newConfig, move(newConfig))
    }

    Stream.cons(config, move(config))
  }

}
