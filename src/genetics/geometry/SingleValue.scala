package genetics.geometry

import scala.util.Random

object SingleValue {

  def costFunction(number: Double): SingleValue => Double = {
    val fitnessTest: SingleValue => Double = (guess: SingleValue) =>
      (guess.value - number).abs
    fitnessTest
  }

  def incubator(genes: List[Double]): SingleValue = {
    val geneVal: Double = genes.head
    new SingleValue(geneVal)
  }

}



class SingleValue(var value: Double){}
