package tests

import genetics.GeneticAlgorithm
import genetics.geometry.SingleValue
import org.scalatest.funsuite.AnyFunSuite

class TestSingleValue extends AnyFunSuite {

  val EPSILON: Double = 0.05

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }


  test("Genetic Algorithm Finds a Random Number") {
    val hiddenNumber = 50.0
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
  test("Genetic alforithm works with different random numbers"){
    val hiddenNumber = 563.23
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
  test("Genetic alforithm works with different random numbers 2"){
    val hiddenNumber = 975.2
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
  test("Genetic alforithm works with different random numbers 3"){
    val hiddenNumber = -953.21
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
  test("Genetic alforithm works with different random numbers 4"){
    val hiddenNumber = -128
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
  test("Genetic alforithm works with different random numbers 5"){
    val hiddenNumber = -501.12
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
  test("Genetic alforithm works with different random numbers 6"){
    val hiddenNumber = -1209.21
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
  test("Genetic alforithm works with different random numbers 7"){
    val hiddenNumber = 0
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
  test("Genetic alforithm works with different random numbers 8"){
    val hiddenNumber = Math.PI
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
  test("Genetic alforithm works with different random numbers 9"){
    val hiddenNumber = Math.E
    val computed = GeneticAlgorithm.geneticAlgorithm(SingleValue.incubator, SingleValue.costFunction(hiddenNumber), 1)
    println(computed.value)
    assert(equalDoubles(hiddenNumber, computed.value))
  }
}
