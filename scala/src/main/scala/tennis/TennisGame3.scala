package tennis

class TennisGame3(val p1N: String, val p2N: String) extends TennisGame {

  var p2: Int = 0
  var p1: Int = 0

  def points: (Int, Int) = (p1, p2)

  def calculateScore(): String = {
    var s: String = ""
    if (points.lestThenFourButNotDeuce) {
      val p = Array("Love", "Fifteen", "Thirty", "Forty")
      s = p(p1)
      if (points.same) s + "-All" else s + "-" + p(p2)
    } else {
      if (points.same) "Deuce"
      else {
        s = if (points.oneGreaterThanTwo) p1N else p2N
        if (points.difference * points.difference == 1) "Advantage " + s else "Win for " + s
      }
    }
  }

  implicit class __PointsOps(points: (Int, Int)) {
    val difference: Int = p1 - p2
    val oneGreaterThanTwo: Boolean = p1 > p2
    val same: Boolean = p1 == p2
    val lestThenFourButNotDeuce: Boolean = p1 < 4 && p2 < 4 && !(p1 + p2 == 6)
  }

  def wonPoint(playerName: String) {
    if (playerName == "player1")
      this.p1 += 1
    else
      this.p2 += 1

  }

}
