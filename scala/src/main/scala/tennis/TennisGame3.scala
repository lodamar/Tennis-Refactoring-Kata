package tennis

class TennisGame3(val p1N: String, val p2N: String) extends TennisGame {

  var p2: Int = 0
  var p1: Int = 0

  def calculateScore(): String =
    if (points.lestThenFourButNotDeuce) {
      if (points.same) pointToScore(p1) + "-All"
      else pointToScore(p1) + "-" + pointToScore(p2)
    } else {
      if (points.same) "Deuce"
      else {
        val s = if (points.oneGreaterThanTwo) p1N else p2N
        if (points.difference * points.difference == 1) "Advantage " + s else "Win for " + s
      }
    }

  val pointToScore: Int => String = {
    case 0 => "Love"
    case 1 => "Fifteen"
    case 2 => "Thirty"
    case 3 => "Forty"
  }

  def points: (Int, Int) = (p1, p2)

  implicit class PointsOps(points: (Int, Int)) {
    val difference: Int = p1 - p2
    val oneGreaterThanTwo: Boolean = p1 > p2
    val same: Boolean = p1 == p2
    val lestThenFourButNotDeuce: Boolean = p1 < 4 && p2 < 4 && !(p1 + p2 == 6)
  }

  def wonPoint(playerName: String): Unit =
    if (playerName == "player1")
      this.p1 += 1
    else
      this.p2 += 1

}
