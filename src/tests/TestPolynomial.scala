package tests

import genetics.GeneticAlgorithm
import genetics.geometry.{Point, Polynomial, SingleValue}
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer

class TestPolynomial extends FunSuite {

  val EPSILON: Double = 0.5

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }


  test("Genetic Algorithm Computes a first degree polynomial") {
    ///////////////////////Test Polynomial y = x
    val testPoints:List[Point] = List(new Point(1,1),new Point(2,2),new Point(3,3),new Point(4,4),new Point(5,5),new Point(6,6),new Point(7,7),new Point(8,8),new Point(9,9))
    val computed = GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator, Polynomial.costFunction(testPoints), 2)
    assert(equalDoubles(computed.coefficients.head,1) && equalDoubles(computed.coefficients(1),0))
  }

  test("Genetic Algorithm computes a second degree polynomial"){
    //////////////////////// y=x^2
    val testPoints:List[Point] = List(new Point(0,0),new Point(1,1),new Point(2,4),new Point(3,9),new Point(4,16),new Point(5,25),new Point(7,49),new Point(8,64),new Point(9,81))
    val computed = GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator, Polynomial.costFunction(testPoints), 3)
    assert(equalDoubles(computed.coefficients.head,1) && equalDoubles(computed.coefficients(1),0)&& equalDoubles(computed.coefficients(2),0))
  }

  test("Genetic algorithm calculates more complicated first degree polynomials"){
    ////////////////////////2.5x+4
    val testPoints:List[Point] = List(new Point(-1,1.5),new Point(0,4),new Point(2,9),new Point(2.5,10.25),new Point(3,11.5),new Point(-1.6,0),new Point(-3.6,-5),new Point(-5.5,-9.75),new Point(-5.4,-9.5))
    val computed = GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator, Polynomial.costFunction(testPoints), 2)
    assert(equalDoubles(computed.coefficients.head,2.5) && equalDoubles(computed.coefficients(1),4))
  }
  test("Algorithm computes more complicated second degree polynomials"){
    ///////////////////////2.5x^2+5x+4
    val testPoints:List[Point] = List(new Point(-1,1.5),new Point(0,4),new Point(1,11.5),new Point(2,24),new Point(3,41.5),new Point(4,64),new Point(5,91.5),new Point(6,124),new Point(7,161.5))
    val computed = GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator, Polynomial.costFunction(testPoints), 3)
    assert(equalDoubles(computed.coefficients.head,2.5) && equalDoubles(computed.coefficients(1),5)&& equalDoubles(computed.coefficients(2),4))
  }
  test("Algorithm computes even more complicated first degree polynomials"){
    ///////////////////////0.123x- 0.214
    val testPoints:List[Point] = List(new Point(-4,-0.706),new Point(-3,-0.583),new Point(-2,-0.46),new Point(-1,-0.337),new Point(0,-0.214),new Point(1,-0.091),new Point(2,0.032),new Point(3,0.155),new Point(4,0.278))
    val computed = GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator, Polynomial.costFunction(testPoints), 2)
    assert(equalDoubles(computed.coefficients.head,0.123) && equalDoubles(computed.coefficients(1),0.214))
  }
  test("Algorithm computes even more complicated second degree polynomials"){
    ///////////////////////0.24x^2+43x-69
    def generatePoints(coefficients:List[Double]): List[Point] ={
      def evaluateY(x: Double): Double = {
        var power: Int = coefficients.length - 1
        var yVal: Double = 0
        for (coefficient <- coefficients) {
          yVal += coefficient * Math.pow(x, power)
          power -= 1
        }
        yVal
      }
      var retVal:ListBuffer[Point] = ListBuffer()
      for (i<- 0 until 10){
        retVal+= new Point(i,evaluateY(i))
      }
      retVal.toList
    }

    val testPoints:List[Point] = generatePoints(List(0.24,43,-69))
    val computed = GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator, Polynomial.costFunction(testPoints), 3)
    assert(equalDoubles(computed.coefficients.head,0.24) && equalDoubles(computed.coefficients(1),43)&& equalDoubles(computed.coefficients(2),-69))
  }
}
