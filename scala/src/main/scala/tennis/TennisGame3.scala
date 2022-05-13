package tennis

class TennisGame3(val p1N: String, val p2N: String) extends TennisGame {

  var p2: Int = 0
  var p1: Int = 0

  def calculateScore(): String = {
    var s: String = ""
    if (lestThenFourButNotDeuce) {
      val p = Array("Love", "Fifteen", "Thirty", "Forty")
      s = p(p1)
      if (same) s + "-All" else s + "-" + p(p2)
    } else {
      if (same) "Deuce"
      else {
        s = if (oneGreaterThanTwo) p1N else p2N
        if ((difference) * (difference) == 1) "Advantage " + s else "Win for " + s
      }
    }
  }

  private def difference = {
    p1 - p2
  }

  private def oneGreaterThanTwo = {
    p1 > p2
  }

  private def same = {
    p1 == p2
  }

  private def lestThenFourButNotDeuce = {
    p1 < 4 && p2 < 4 && !(p1 + p2 == 6)
  }

  def wonPoint(playerName: String) {
    if (playerName == "player1")
      this.p1 += 1
    else
      this.p2 += 1

  }

}
