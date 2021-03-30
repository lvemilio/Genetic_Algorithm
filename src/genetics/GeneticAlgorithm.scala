package genetics

import com.google.common.collect.MultimapBuilder.ListMultimapBuilder

import javax.swing.event.ListDataEvent
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}


object GeneticAlgorithm {

  /**
   * Uses a genetic algorithm to optimize a generic problem
   *
   * @param incubator     Determines how instances of type T are created from a List of Doubles (bestGenes)
   * @param costFunction  Determines the cost for a given instance of T
   * @param numberOfGenes The size of the List expected by the incubator
   * @tparam T The type to be optimized
   * @return An instance of T with minimal cost
   */
  def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {
    val ANIMALS: Int = 100
    val GENERATIONS: Int = 1000000
    val BEST_ANIMALS: Int = 3

    val r = Random
    val bestGenes: ListBuffer[List[Double]] = ListBuffer.fill(BEST_ANIMALS)(List.fill(numberOfGenes)(r.nextFloat() * 1000))


    for (i <- 1 to ANIMALS) {
      val genes: ListBuffer[Double] = ListBuffer.fill(numberOfGenes)(r.nextFloat() * 1000)
      val newAnimal: T = incubator(genes.toList)
      val newAnimalCost: Double = costFunction(newAnimal)
      breakable {
        for (j <- bestGenes.indices) {
          val thisAnimal: T = incubator(bestGenes(j))
          val thisAnimalCost: Double = costFunction(thisAnimal)
          if (newAnimalCost < thisAnimalCost) {
            println("Better genes found:" + genes)
            bestGenes -= bestGenes(j)
            bestGenes += genes.toList
            println(bestGenes)
            break()
          }
        }
      }
    }


    println("Best Genes:" + bestGenes)


    var finalGenes: List[Double] = List()
    val childGenes = crossover(bestGenes).toList
    val child = incubator(childGenes)
    val childCost: Double = costFunction(child)

    for (gene <- bestGenes) {
      val thisGeneCost: Double = costFunction(incubator(gene))
      if (childCost < thisGeneCost) {
        finalGenes = childGenes
      }
      else {
        finalGenes = gene
      }
    }
    println("Final genes:" + finalGenes)
    var finalAnimal: T = incubator(finalGenes)


    breakable {
      for (animalInd <- 0 until ANIMALS) {
        val newGenes: ListBuffer[Double] = ListBuffer.fill(numberOfGenes)(r.nextFloat() * 10000000)
        val newAnimal: T = incubator(newGenes.toList)
        if (costFunction(newAnimal).abs < costFunction(finalAnimal).abs) {
          finalGenes = newGenes.toList
          finalAnimal = newAnimal
          println("Better animal found, animal genes:" + newGenes)
        }
      }


      for (i <- 1 to GENERATIONS) {
        val finalAnimalCost: Double = costFunction(finalAnimal)
        if (finalAnimalCost <= 0.05) {
          break()
        }
        var newGenes = mutateGenes(finalGenes, finalAnimalCost)

        val newAnimal: T = incubator(newGenes)
        val newAnimaCost: Double = costFunction(newAnimal)
        if (newAnimaCost < finalAnimalCost) {
          finalGenes = newGenes
          finalAnimal = newAnimal
          newGenes = List()
        }
        else {
          newGenes = List()
        }
        println("Generation:" + i + ". Best genes: " + finalGenes + ". Cost:" + costFunction(incubator(finalGenes)))
      }
    }
    finalAnimal
  }


  def crossover(genes: ListBuffer[List[Double]]): ListBuffer[Double] = {
    var childGenes: ListBuffer[Double] = ListBuffer()
    var geneAvg: Double = 0
    for (i <- genes.indices) {
      for (j <- genes.head.indices) {
        val thisAllele: Double = genes(i)(j)
        geneAvg += thisAllele
        if (i == genes.length - 1) {
          println("This gene avg =" + geneAvg)
          val childGene: Double = geneAvg / genes.length
          geneAvg = 0
          childGenes += childGene
        }
      }
    }
    childGenes
  }

  def mutateGenes(finalGenes: List[Double], finalAnimalCost: Double): List[Double] = {
    val r = Random
    var newGenes: ListBuffer[Double] = ListBuffer()


    for (allele <- finalGenes) {
      if (r.nextFloat() >= 0.7) {
        newGenes += allele
      }

      else {
        if (finalAnimalCost == Double.PositiveInfinity){
          if(r.nextFloat() >= 0.5) {
            newGenes += r.nextFloat()
          }
          else if(r.nextFloat() >= 0.5) {
            newGenes += r.nextFloat()*100
          }
          else{
            newGenes += r.nextFloat()*1000
          }
        }
        else if (finalAnimalCost <= 10 && finalAnimalCost >= 5) {
          if (r.nextFloat() <= 0.5) {
            newGenes += allele - r.nextFloat()
          }
          else {
            newGenes += allele + r.nextFloat()
          }
        }

        else if (finalAnimalCost < 5 && finalAnimalCost >= 1) {
         if(r.nextFloat() <= 0.5) {
          if (r.nextFloat() <= 0.5) {
            newGenes += allele - (r.nextFloat() / 10)
          }
          else {
            newGenes += allele + (r.nextFloat() / 10)
          }
         }

          else{
           if (r.nextFloat() <= 0.5) {
             newGenes += allele - r.nextFloat() *10
           }
           else {
             newGenes += allele + r.nextFloat() *10
           }
         }
        }

        else if (finalAnimalCost < 1 && finalAnimalCost >= 0.05) {
          if (r.nextFloat() <= 0.5) {
            if (r.nextFloat() <= 0.5) {
              newGenes += allele - r.nextFloat() / 100
            }
            else {
              newGenes += allele + r.nextFloat() / 100
            }
          }
          else{
            if (r.nextFloat() <= 0.5) {
              newGenes += allele - r.nextFloat()*10
            }
            else {
              newGenes += allele + r.nextFloat()*10
            }
          }
        }

        else {
          if (r.nextFloat() <= 0.5) {
            if (r.nextFloat() <= 0.5) {
              newGenes += allele - r.nextFloat() * 10
            }
            else {
              newGenes += allele + r.nextFloat() * 10
            }
          }
          else if (r.nextFloat() <= 0.5) {
            if (r.nextFloat() <= 0.5) {
              newGenes += allele - r.nextFloat()
            }
            else {
              newGenes += allele + r.nextFloat()
            }
          }
          else if (r.nextFloat() <= 0.5) {
            if (r.nextFloat() <= 0.5) {
              newGenes += allele - r.nextFloat() / 10
            }
            else {
              newGenes += allele + r.nextFloat() / 10
            }
          }
          else {
            if (r.nextFloat() <= 0.5) {
              newGenes += allele - r.nextFloat() / 100
            }
            else {
              newGenes += allele + r.nextFloat() / 100
            }
          }
        }
      }
    }
    newGenes.toList
  }


}

