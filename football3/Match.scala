package o1.football3

import scala.collection.mutable.Buffer
import scala.math._



class Match(val home: Club, val away: Club) {

  private val homeScorers = Buffer[Player]()    // container: goalscorers of the home team are added here
  private val awayScorers = Buffer[Player]()    // container: goalscorers of the away team are added here

  def location = this.home.stadium

  def allScorers = {
    var buffer = Buffer[Player]()
    var luku1 = this.homeScorers.size
    var luku2 = this.awayScorers.size
    var n = 0
    var m = 0

    while (n < luku1) {
      buffer += this.homeScorers(n)
      n += 1
    }

    while (m < luku2) {
      buffer += this.awayScorers(m)
      m += 1
    }

    val buffer1 = buffer.to(Vector)
    buffer1
  }

  def goalDifference = {
    this.homeGoals - this.awayGoals
  }

  def awayGoals = {
    this.awayScorers.size
  }

  def homeGoals = {
    this.homeScorers.size
  }

  def hasScorer(player: Player) = {
    if (player.employer == this.home) {
      this.homeScorers.contains(player)
    } else {
      this.awayScorers.contains(player)
    }
  }

  def isAwayWin = {
    this.homeGoals < this.awayGoals
  }

  def isHomeWin = {
    this.homeGoals > this.awayGoals
  }

  def isGoalless = {
    this.totalGoals == 0
  }

  def isHigherScoringThan(peli: Match) = {
     this.totalGoals > peli.totalGoals
  }

  def isTied = {
     this.homeGoals == this.awayGoals
  }

  def totalGoals = {
    this.homeGoals + this.awayGoals
  }

  def winnerName = {
    val tulos = this.winner match {
      case None => "no winner"
      case Some(klubi) => klubi.name
    }
    tulos
  }

  def winner = {
    if (this.isHomeWin) {
      Some(this.home)
    } else if (this.isAwayWin) {
      Some(this.away)
    } else {
      None
    }
  }

  def winningScorer = {
    if (this.isTied) {
      None
    } else {
      if (this.isHomeWin) {
        Some(this.homeScorers(this.awayGoals))
      } else {
        Some(this.awayScorers(this.homeGoals))
      }
    }
  }

  def addGoal(scorer: Player): Unit = {
    if (scorer.employer == this.home) {
      this.homeScorers += scorer
    } else {
      this.awayScorers += scorer
    }
  }

  override def toString = {
    if (this.isTied) {
      if (this.totalGoals == 0) {
        s"${this.home.name} vs. ${this.away.name} at ${this.home.stadium}: tied at nil-nil"
      } else {
      s"${this.home.name} vs. ${this.away.name} at ${this.home.stadium}: tied at ${this.homeGoals}-all"
      }
    }

    else {
      if (this.homeGoals > this.awayGoals) {
        s"${this.home.name} vs. ${this.away.name} at ${this.home.stadium}: ${this.homeGoals}-${this.awayGoals} to ${this.home.name}"
      } else {
        s"${this.home.name} vs. ${this.away.name} at ${this.home.stadium}: ${this.awayGoals}-${this.homeGoals} to ${this.away.name}"
      }
    }
  }


}