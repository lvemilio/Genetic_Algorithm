package tests

import genetics.GeneticAlgorithm
import genetics.aimbot.{AimBot, PhysicsVector}
import genetics.geometry.Polynomial
import org.scalatest.funsuite.AnyFunSuite
import play.api.libs.json.jackson.PlayJsonModule

class TestAimBot extends AnyFunSuite {

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

    assert(equalDoubles(0,computed.normal2d().x) && equalDoubles(1,computed.normal2d().y))
  }
  test("Genetic Algorithms Hits a Moving target moving in a different pattern") {

    val geneCount = AimBot.numberOfDimensions
    val sourceLoca:PhysicsVector = new PhysicsVector(4,0)
    val targetLoca:PhysicsVector = new PhysicsVector(0,4)
    val targetVel:PhysicsVector = new PhysicsVector(4,3)
    val anwser:PhysicsVector = new PhysicsVector(0,1)

    val computed = GeneticAlgorithm.geneticAlgorithm(AimBot.incubator, AimBot.costFunction(sourceLoca,targetLoca,targetVel), geneCount)
    assert(equalDoubles(0,computed.normal2d().x) && equalDoubles(1,computed.normal2d().y))
  }
  test("Genetic Algorithms Hits a Moving target moving away from source") {

    val geneCount = AimBot.numberOfDimensions
    val sourceLoca:PhysicsVector = new PhysicsVector(0,0)
    val targetLoca:PhysicsVector = new PhysicsVector(3,3)
    val targetVel:PhysicsVector = new PhysicsVector(1,1)
    val anwser:PhysicsVector = new PhysicsVector(1,1)

    val computed = GeneticAlgorithm.geneticAlgorithm(AimBot.incubator, AimBot.costFunction(sourceLoca,targetLoca,targetVel), geneCount)

    assert(equalDoubles(computed.normal2d().x,computed.normal2d().y) && computed.normal2d().x>0)
  }
  test("Genetic algorithm finds a target moving towards target"){
    val geneCount = AimBot.numberOfDimensions
    val sourceLoca:PhysicsVector = new PhysicsVector(0,0)
    val targetLoca:PhysicsVector = new PhysicsVector(3,3)
    val targetVel:PhysicsVector = new PhysicsVector(-1,-1)
    val anwser:PhysicsVector = new PhysicsVector(1,1)

    val computed = GeneticAlgorithm.geneticAlgorithm(AimBot.incubator, AimBot.costFunction(sourceLoca,targetLoca,targetVel), geneCount)
    assert(equalDoubles(computed.normal2d().x,computed.normal2d().y) && computed.normal2d().x>0)
  }
  test("Genetic algorithm hits moving target moving away with negative coordinates"){
    val geneCount = AimBot.numberOfDimensions
    val sourceLoca:PhysicsVector = new PhysicsVector(-3,0)
    val targetLoca:PhysicsVector = new PhysicsVector(-2,-5)
    val targetVel:PhysicsVector = new PhysicsVector(-1,-2)
    val anwser:PhysicsVector = new PhysicsVector(0,-1)

    val computed = GeneticAlgorithm.geneticAlgorithm(AimBot.incubator, AimBot.costFunction(sourceLoca,targetLoca,targetVel), geneCount)
    assert(equalDoubles(computed.normal2d().x,0) && equalDoubles(computed.normal2d().y,-1))
  }
  test("Genetic algorithm hits moving by a target"){
    val geneCount = AimBot.numberOfDimensions
    val sourceLoca:PhysicsVector = new PhysicsVector(1,0)
    val targetLoca:PhysicsVector = new PhysicsVector(0,-8)
    val targetVel:PhysicsVector = new PhysicsVector(1,1)
    val anwser:PhysicsVector = new PhysicsVector(0,-1)

    val computed = GeneticAlgorithm.geneticAlgorithm(AimBot.incubator, AimBot.costFunction(sourceLoca,targetLoca,targetVel), geneCount)
    assert(equalDoubles(computed.normal2d().x,0) && equalDoubles(computed.normal2d().y,-1))
  }
}
