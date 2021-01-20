package o1.football3

import scala.collection.mutable.Buffer
import scala.math._


class Season() {

  private val matsit = Buffer[Match]()

  def addResult(newResult: Match) = {
    this.matsit += newResult
  }

  def biggestWin = {
    if (this.matsit.isEmpty) {
      None
    } else {
      var luku = this.matsit.size
      var n = 0
      var maksimi = -5
      var tieto: Option[Match] = None

      while (n < luku) {
        if (abs(this.matsit(n).goalDifference) > maksimi) {
          maksimi = abs(this.matsit(n).goalDifference)
          tieto = Some(this.matsit(n))
        }
        n += 1
      }
      tieto
    }
  }

  def latestMatch = {
    if (this.matsit.isEmpty) {
      None
    } else {
      Option(this.matsit.last)
    }
  }

  def matchNumber(number: Int) = {
    this.matsit.lift(number)
  }

  def numberOfMatches = {
    this.matsit.size
  }



}