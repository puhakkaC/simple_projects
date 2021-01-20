package o1.peeveli

import GameState.Unrevealed


/** Each instance of class `GameState` represents a single state within the Peeveli variant of
  * Hangman: What does the (partially visible) target word look like to the guesser? How many
  * incorrect guesses can the guesser still make? Which guesses have already been made? Moreover,
  * our dishonest hangman needs an additional piece of information: Which words are still credible
  * solutions given the earlier guesses?
  */
class GameState(val missesAllowed: Int, val previousGuesses: String, val visibleWord: String, val possibleSolutions: Vector[String])  {

  /** Creates a new `GameState` that represents the initial state of a new game of Peeveli.
  */
  def this(missesAllowed: Int, length: Int, vocabulary: Vector[String]) = {
    this(missesAllowed, "", Unrevealed.toString * length, vocabulary.map( _.toUpperCase )) 
  }


  /** Returns the length of the target word. */
  def wordLength = this.visibleWord.length


  /** Returns the number of all known words that are (still) possible solutions to this
    * game of Peeveli, given the guesses that have already been made. */
  def numberOfSolutions = this.possibleSolutions.size


  /** Returns `true` if the player has missed with more guesses than allowed and has therefore
    * lost the game; returns `false` otherwise. */
  def isLost = this.missesAllowed < 0


  /** Returns `true` if the guesser has won the game. Returns `false` otherwise. */
  def isWon = !this.isLost && this.visibleWord.forall(_ != '_')


  /** Returns a version of the currently visible target word so that additional characters are
    * revealed as indicated by the given pattern. */
  private def reveal(patternToReveal: String) = {
    var sana = ""
    for (i <- 0 until this.visibleWord.length) {
      if (patternToReveal.apply(i) != '_') {
        sana += patternToReveal.apply(i)
      } else {
        sana += this.visibleWord(i)
      }
    }
    sana
  }


  /** Returns a new `GameState` that follows this current one given that the guesser guesses a
    * particular letter. */
  def guessLetter(guess: Char) = {
    val actualGuess = guess.toUpper

    var indeksit = Map[String, Vector[Int]]()
    var i = Vector[Int]()


    for (sana <- this.possibleSolutions) {
      var n = sana.toUpperCase.indexOf(actualGuess,0)

      if (n == -1) {
        i = i :+ n
      } else {

        while (n >= 0) {
          i = i :+ n
          n = sana.toUpperCase.indexOf(actualGuess,n + 1)
        }
      }

      indeksit += sana -> i
      i = i.empty
    }

    var (indeksit2,indeksit3) = indeksit.toVector.unzip
    var haluttu = indeksit3.groupBy(indeksit => indeksit).map(pari => pari._1 -> pari._2.size).toMap.toVector.sortBy(avain => -avain._2).apply(0)._1
    var (solutions,ei) = indeksit.toVector.filter(_._2 == haluttu).unzip

    var nakyva = ""

    for (x <- 0 until this.visibleWord.length) {
      if (haluttu.contains(x)) {
        nakyva += actualGuess
      } else {
        nakyva += '_'
      }
    }

    if (haluttu == Vector[Int](-1)) {
      new GameState(this.missesAllowed-1, this.previousGuesses + actualGuess, this.reveal(nakyva), solutions)
    } else {
      new GameState(this.missesAllowed, this.previousGuesses + actualGuess, this.reveal(nakyva), solutions)
    }
  }


  /** Returns a string description of the game state. */
  override def toString =
    this.visibleWord + ", " +
      "missed guesses allowed: " + this.missesAllowed + ", " +
      "guesses made: " + (if (this.previousGuesses.isEmpty) "none" else this.previousGuesses) + ", " +
      "solutions left: " + this.numberOfSolutions

}


object GameState {

  /** the character that is used by Peeveli to signify unrevealed letters */
  val Unrevealed = '_'

}
