package genetics.geometry

import scala.collection.mutable.ListBuffer
import scala.util.Random


object Polynomial {

  def costFunction(points: List[Point]): Polynomial => Double = {
    def polynomialCost():Polynomial=>Double = (guess:Polynomial)=> {
      var totalCost:Double = 0
      var yVals:ListBuffer[Double] = ListBuffer()
      ////////////////Calculate guessed y
      for(point<-points){
        val yVal:Double = guess.evaluateY(point.x)
        yVals += yVal

      }
      ////////////////Calculate point difference(X not used because they are respectively the same)
      for (pointInd<- points.indices){
        val pointY:Double = points(pointInd).y
        val guessedY:Double = yVals(pointInd)

        if (pointY>guessedY){
          totalCost+= pointY - guessedY
        }else{
          totalCost+= guessedY - pointY
        }

      }
      totalCost.abs
    }
    polynomialCost()
  }

  def incubator(genes: List[Double]): Polynomial = {
    new Polynomial(genes)
  }
}

/**
  * Represents a polynomial given its coefficients ending with the constant coefficient
  *
  * Ex. new Polynomial(List(1.5, -2.2, 5)) represents 1.5*pow(x, 2) - 2.2*x + 5
  *
  */
class Polynomial(var coefficients: List[Double]) {
  def evaluateY(x: Double): Double = {
    var power:Int = coefficients.length - 1
    var yVal:Double = 0
    for(coefficient<-coefficients){
      yVal += coefficient * Math.pow(x,power)
      power -= 1
    }
    yVal
    }
  }

