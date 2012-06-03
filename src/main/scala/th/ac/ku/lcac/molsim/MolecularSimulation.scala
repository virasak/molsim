package th.ac.ku.lcac.molsim

import th.ac.ku.lcac.mc._
import scala.util.Random

case class Position(x: Double, y: Double, z: Double)

object Position {
  def distance(a: Position, b: Position): Double = math.sqrt(a.x*b.x + a.y*b.y + a.z*b.z)
}


class ArMC(val maxStepSize: Double, acceptRate: Double = 0.5, randomSeed: Option[Long] = None) extends MC[List[Position]] {

  val epsilon = 119.8 // K

  val zigma = 0.3405 // nm

  val random = randomSeed match {
    case Some(seed) => new Random(seed)
    case None       => new Random()
  }

  def accept(o: List[Position], n: List[Position]): Boolean = (energy(n) < energy(o)) || random.nextDouble > acceptRate

  // Random walk
  def move(list: List[Position]): List[Position] = {
      for (atom <- list) yield Position(atom.x + walk, atom.y + walk, atom.z + walk)
  }

  // random walk between -maxStepSize to maxStepSize
  private def walk: Double = (2.0*random.nextDouble - 1.0) * maxStepSize


  private def energy(m: List[Position]): Double = {
    def calc(m: List[Position], e: Double): Double = m match {
      case Nil => e
      case a::others =>
        val zorList = for (b <- others) yield zigma/Position.distance(a, b)
        val new_e = zorList.foldLeft(e) { (eacc, zor) => eacc + (4 * epsilon * (math.pow(zor, 12) - math.pow(zor, 6))) }

        calc(others, new_e)
    }

    calc(m, 0.0)

  }
}

object MolecularSimulation {

  def main(args: Array[String]) {

    val atoms = List(
      Position(0.0, 0.0, 0.0),
      Position(0.96, 0.0, 0.0)
    )

    implicit val xxx = new ArMC(0.1)

    for (t <- MC.compose(atoms).take(10)) {
      println(t)
    }

  }
}

