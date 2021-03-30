package genetics.aimbot

import scala.collection.mutable.ListBuffer

object AimBot{

  // set this to the number of genes you'll use in your incubator method
  val numberOfDimensions: Int = 2

  // Any returned velocity must have this magnitude
  // do not change this value
  val projectileSpeed: Double = 7.0


  def costFunction(sourceLocation: PhysicsVector, targetLocation: PhysicsVector, targetVelocity: PhysicsVector): PhysicsVector => Double = {
    def calculateVector:PhysicsVector=>Double = (guess:PhysicsVector)=>{
     ////////////////////////Find appropriate vectors
     val distanceToTarget:PhysicsVector = new PhysicsVector(targetLocation.x-sourceLocation.x,targetLocation.y-sourceLocation.y)
     val projectileVelocity:PhysicsVector = new PhysicsVector(guess.x-sourceLocation.x,guess.y-sourceLocation.y)

      val projectileVelocityNormal:PhysicsVector = projectileVelocity.normal2d()
      val targetVelocityNormal:PhysicsVector = targetVelocity.normal2d()

      val as:PhysicsVector = sourceLocation
      val ad:PhysicsVector = projectileVelocityNormal

      val bs:PhysicsVector = targetLocation
      val bd:PhysicsVector = targetVelocityNormal

      val u:Double = ((as.y*bd.x + bd.y*bs.x - bs.y*bd.x - bd.y*as.x ) / (ad.x*bd.y - ad.y*bd.x))
      val v:Double = ((as.x + ad.x * u - bs.x) / bd.x)

      ///////////////////////Checking if distance is paralel to velocity
      if(distanceToTarget.x/targetVelocity.x == distanceToTarget.y/targetVelocity.y){
        val scalarProduct:Double = distanceToTarget.x*guess.x + distanceToTarget.y*guess.y
        val distanceMagnitude:Double = distanceToTarget.magnitude()
        val projectileMagnitude:Double = guess.magnitude()

        val angle:Double = Math.acos(scalarProduct/(distanceMagnitude*projectileMagnitude))
        val cost:Double = angle*100
        cost
      }
      else if(u>0 && v>0 ){
        //////////////////////If projectiles are flying towards each other
          val intersectionPoint:PhysicsVector = new PhysicsVector(as.x + ad.x * u,as.y + ad.y*u)
          val projectileToIntersection:PhysicsVector = new PhysicsVector(intersectionPoint.x-sourceLocation.x,intersectionPoint.y-sourceLocation.y)
          val targetToIntersection:PhysicsVector = new PhysicsVector(intersectionPoint.x-targetLocation.x,intersectionPoint.y-targetLocation.y)


          val projectileTravelTime:Double = projectileToIntersection.magnitude()/projectileSpeed
          val targetTravelTime:Double = targetToIntersection.magnitude()/targetVelocity.magnitude()
          val cost:Double = (projectileTravelTime - targetTravelTime).abs
          cost
      }
      else{
        println("Vectors a:" + projectileVelocity.toString + " b:" + targetVelocity.toString+" do not meet")
          Double.PositiveInfinity
      }
    }
  calculateVector
  }


  def incubator(genes: List[Double]): PhysicsVector = {
    new PhysicsVector(genes.head,genes(1))
  }

}
