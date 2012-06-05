package th.ac.ku.lcac.molsim

import th.ac.ku.lcac.mc._
import scala.util.Random

case class Position(x: Double, y: Double, z: Double) {
  override def toString() = "{%12.6f, %12.6f, %12.6f}".format(x,y,z)
}

object Position {
  def distance(a: Position, b: Position): Double = {
    val dx = a.x - b.x
    val dy = a.y - b.y
    val dz = a.z - b.z
    math.sqrt(dx*dx + dy*dy + dz*dz)
  }

}


class ArMC(val maxStepSize: Double, val temperature: Double = 300, randomSeed: Option[Long] = None) extends MC[List[Position]] {

  val BoltzmanConstant: Double = 1.380648813E-23 // J/K

  val Beta = 1.0/(BoltzmanConstant*temperature) // 1/J

  val epsilon = 1.65E-21 // J

  val zigma = 3.405 // Angstrom

  val random = randomSeed match {
    case Some(seed) => new Random(seed)
    case None       => new Random()
  }

  def accept(o: List[Position], n: List[Position]): Boolean = {
    val dV = energy(n) - energy(o)
    (dV < 0) || random.nextDouble < math.min(1.0, math.exp(- Beta*dV))
  }

  // Random move of the configuration
  // One position per move
  def move(list: List[Position]): List[Position] = {
      // pre & post is fixed positions.
      // m is moving position
      val (pre, m::post) = list.splitAt(random.nextInt(list.size))
      // random move in all three direction
      val n = Position(m.x + walk, m.y + walk, m.z + walk)
      pre++(n::post)
  }

  // random walk between -maxStepSize to maxStepSize
  private def walk: Double = (2.0*random.nextDouble - 1.0) * maxStepSize


  // Energy of the system (J).
  private def energy(m: List[Position]): Double = {

    def calc(a: Position, others: List[Position], acc: Double): Double = others match {
      case Nil => acc
      case bs  =>
        val zorList = for (b <- bs) yield zigma/Position.distance(a, b)
        val new_e = zorList.foldLeft(acc) { (eacc, zor) => eacc + (4 * epsilon * (math.pow(zor, 12) - math.pow(zor, 6))) }

        calc(bs.head, bs.tail, new_e)
    }

    m match {
      case Nil    => 0.0
      case x::xs  => calc(x, xs, 0.0)
    }

  }
}

object MolecularSimulation {

  def main(args: Array[String]) {

    val atoms = List(
      Position(0.0, 0.0, 0.0),
      Position(3.0, 0.0, 0.0)
    )

    implicit val xxx = new ArMC(1.0)

    val transientSteps = 100
    val equilibriumSteps = 100

    for (config <- MC.compose(atoms).drop(transientSteps).take(equilibriumSteps)) {
      println(Position.distance(config.head, config.tail.head))
    }

  }
}

