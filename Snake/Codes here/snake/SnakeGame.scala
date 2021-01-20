package o1.snake

import o1._
import scala.util.Random
import SnakeGame._


// This companion object provides aConstants and an utility function.
object SnakeGame {

  // The height and width of the game area
  val SizeInSquares = 40

  // Randomly returns the position of a single square on the grid, excluding the
  // very edges (where no food can appear and which kill the snake if it enters).
  //TÄMÄ ANNETTUNA EI OMA TEKEMÄ
  def randomLocationOnGrid() = {
    new GridPos(Random.nextInt(SizeInSquares - 2) + 1,
                Random.nextInt(SizeInSquares - 2) + 1)
  }

}



// Represents games of Snake. 
class SnakeGame(initialPos: GridPos, initialHeading: CompassDir) {

  private var segments = Vector(initialPos) // container: the locations of every segment of the snake, in order from head to tail
  var snakeHeading = initialHeading         // most-recent holder (the direction most recently set for the snake)
  var nextFood = randomLocationOnGrid()     // most-recent holder (a changing random location for the next available food item)

  def snakeSegments = this.segments

  def isOver = {
    val head = this.segments.head
    val validCoords = 1 until SizeInSquares
    val collidedWithWall = !validCoords.contains(head.x) || !validCoords.contains(head.y) || (this.segments.drop(1).contains(this.segments.head))

    collidedWithWall
  }

  // This gets repeatedly called as the game progresses. It advances the snake by
  // one square in its current heading. In case the snake finds food, it grows by
  // one segment, the current nextFood vanishes and new food is placed in a random location.
  //TÄMÄ ANNETTUNA EI OMA TEKEMÄ
  def advance() = {
    if (this.segments.head.neighbor(this.snakeHeading) == this.nextFood) {
      this.nextFood = randomLocationOnGrid()
      this.segments = this.segments.head.neighbor(this.snakeHeading) +: this.segments
    } else {
      this.segments = this.segments.head.neighbor(this.snakeHeading) +: segments.take(this.segments.size-1)
    }
  }


}
