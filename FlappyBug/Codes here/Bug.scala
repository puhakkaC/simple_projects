package o1

import o1.constants.BugRadius

import scala.collection.mutable.Buffer
import scala.math._

class Bug(private var pos1: Pos) {

  def pos = this.pos1

  val radius = BugRadius
  private var yVelocity = 0.0

  def flap(y: Double) = {
    this.yVelocity = -y
  }

  def fall() = {
    if (this.pos.y < 350)
      this.yVelocity = this.yVelocity + 2

    this.move(this.yVelocity)
  }

  def move(y: Double) = {
    this.pos1 = this.pos1.addY(y).clampY(0, 350)
  }

  def isInBounds = !(this.pos.y <= 0) && !(this.pos.y >= 350)

  override def toString = s"center at (${pos.x},${pos.y}), radius $radius"

}
