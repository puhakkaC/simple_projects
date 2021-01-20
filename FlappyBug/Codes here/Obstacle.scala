package o1

import constants._
import scala.util.Random
import scala.collection.mutable.Buffer


class Obstacle(val radius: Int) {

  private var pos1 = this.randomLaunchPosition()
  def pos = this.pos1

  def approach() = {
    if (!(this.isActive)) {
      this.pos1 = this.randomLaunchPosition()
    }

    this.pos1 = this.pos1.addX(-ObstacleSpeed)
  }

  def touches(a: Bug) = {
    var sade = this.radius + a.radius
    this.pos.distance(a.pos) < sade

  }

  def isActive = {
    this.pos.x >= -this.radius
  }

  override def toString = "center at " + this.pos + ", radius " + this.radius

  private def randomLaunchPosition() = {
    val launchX = 1000 + this.radius + Random.nextInt(500)
    val launchY = Random.nextInt(400)
    new Pos(launchX, launchY)
  }

}
