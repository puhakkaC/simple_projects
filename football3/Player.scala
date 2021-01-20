package o1.football3


/** The class `Player` represents football players in a match statistics program.
  */
class Player(val name: String, val employer: Club) {

  override def toString = this.name + " (" + this.employer + ")"

}
