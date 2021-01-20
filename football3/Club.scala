package o1.football3


/** The class `Club` represents football clubs in a match statistics system. */
  
class Club(val name: String, val abbreviation: String, val stadium: String) {

  /** Produces a textual description of the club. */
  override def toString = this.name

}
