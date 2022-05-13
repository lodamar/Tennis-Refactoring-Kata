package tennis

class TennisGame3(val p1N: String, val p2N: String) extends TennisGame {

  var p2: Int = 0
  var p1: Int = 0

  def calculateScore(): String =
    if (points.lestThenFourButNotDeuce) {
      if (points.areSame) pointToScore(p1) + "-All"
      else points.map(pointToScore(_) + "-" + pointToScore(_))
    } else {
      if (points.areSame) "Deuce"
      else {
        val player = points.map((p1, p2) => if (p1 > p2) p1N else p2N)
        if (points differenceIs 1) "Advantage " + player
        else "Win for " + player
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
    def map[A](f: (Int, Int) => A): A = f(points._1, points._2)
    def are(p: (Int, Int) => Boolean): Boolean = p(points._1, points._2)
    val lestThenFourButNotDeuce: Boolean = are((p1, p2) => p1 < 4 && p2 < 4 && !(p1 + p2 == 6))
    val areSame: Boolean = are(_ == _)
    val difference: Int = p1 - p2
    def differenceIs(i: Int): Boolean = points.difference * points.difference == i
  }

  def wonPoint(playerName: String): Unit =
    if (playerName == "player1")
      this.p1 += 1
    else
      this.p2 += 1

}
