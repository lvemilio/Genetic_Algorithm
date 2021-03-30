package tests

import genetics.GeneticAlgorithm
import genetics.aimbot.{AimBot, PhysicsVector}
import genetics.geometry.Polynomial
import org.scalatest.FunSuite
import play.api.libs.json.jackson.PlayJsonModule

class TestAimBot extends FunSuite {

  val EPSILON: Double = 0.05

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }


  test("Genetic Algorithms Hits a Moving target") {
    // use this to get the number of genes to use.
    // Do not hardcode a value since the solutions in AutoGrader might not use that value
    val geneCount = AimBot.numberOfDimensions
    val sourceLoca:PhysicsVector = new PhysicsVector(0,0)
    val targetLoca:PhysicsVector = new PhysicsVector(1,7)
    val targetVel:PhysicsVector = new PhysicsVector(-1,0)
    val anwser:PhysicsVector = new PhysicsVector(0,1)

    val computed = GeneticAlgorithm.geneticAlgorithm(AimBot.incubator, AimBot.costFunction(sourceLoca,targetLoca,targetVel), geneCount)

    println(computed)
  }
  test("Genetic Algorithms Hits a Moving target moving in a different pattern") {
    // use this to get the number of genes to use.
    // Do not hardcode a value since the solutions in AutoGrader might not use that value
    val geneCount = AimBot.numberOfDimensions
    val sourceLoca:PhysicsVector = new PhysicsVector(5,43)
    val targetLoca:PhysicsVector = new PhysicsVector(3,3)
    val targetVel:PhysicsVector = new PhysicsVector(1,1)

    val computed = GeneticAlgorithm.geneticAlgorithm(AimBot.incubator, AimBot.costFunction(sourceLoca,targetLoca,targetVel), geneCount)

    println(computed)
  }
  test("Genetic Algorithms Hits a Moving target moving towards source") {
    // use this to get the number of genes to use.
    // Do not hardcode a value since the solutions in AutoGrader might not use that value
    val geneCount = AimBot.numberOfDimensions
    val sourceLoca:PhysicsVector = new PhysicsVector(0,0)
    val targetLoca:PhysicsVector = new PhysicsVector(3,3)
    val targetVel:PhysicsVector = new PhysicsVector(1,1)
    val anwser:PhysicsVector = new PhysicsVector(1,1)

    val computed = GeneticAlgorithm.geneticAlgorithm(AimBot.incubator, AimBot.costFunction(sourceLoca,targetLoca,targetVel), geneCount)

    println(computed)
  }
}
