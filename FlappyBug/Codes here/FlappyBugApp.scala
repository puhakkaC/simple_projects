package o1

import constants._


object FlappyBugApp extends App {

  val sky        = rectangle(ViewWidth, ViewHeight,  LightBlue)
  val ground     = rectangle(ViewWidth, GroundDepth, SandyBrown)
  val trunk      = rectangle(30, 250, SaddleBrown)
  val foliage    = circle(200, ForestGreen)
  val tree       = trunk.onto(foliage, TopCenter, Center)
  val rootedTree = tree.onto(ground, BottomCenter, new Pos(ViewWidth / 2, 30))
  val scenery    = sky.place(rootedTree, BottomLeft, BottomLeft)


  val bugPic = Pic("ladybug.png")


  def rockPic(obstacle: Obstacle) = circle(obstacle.radius * 2, Black)


  val peli = new Game

  val gui = new View(peli, "FlappyBug") {

    def makePic = {
      var otokka = background.place(bugPic, peli.bug.pos)
      peli.obstacles.foldLeft(otokka)((otokka, este) => otokka.place(rockPic(este), este.pos))
    }

    override def onKeyDown(painettu: Key) = {
      if (painettu == Key.Space)
        peli.activateBug()
    }

    override def onTick() = {
      peli.timePasses()
      this.background = this.background.shiftLeft(2)
    }

    var background = scenery

    override def isDone = peli.isLost

  }

  gui.start()

}

