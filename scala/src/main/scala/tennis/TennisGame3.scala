package tennis

class TennisGame3(val p1N: String, val p2N: String) extends TennisGame {

  var p2: Int = 0
  var p1: Int = 0

  def calculateScore(): String = {
    val analysis: PointsAnalysis =
      if (points.lestThenFourButNotDeuce) {
        if (points.areSame) Same(points)
        else Else(points)
      } else {
        if (points.areSame) Deuce
        else {
          val player = points.map((p1, p2) => if (p1 > p2) p1N else p2N)
          if (points differenceIs 1) Advantage(player)
          else Win(player)
        }
      }

    analysis match {
      case Same(points)      => points.map((p1, _) => pointToScore(p1) + "-All")
      case Else(points)      => points.map(pointToScore(_) + "-" + pointToScore(_))
      case Deuce             => "Deuce"
      case Advantage(player) => "Advantage " + player
      case Win(player)       => "Win for " + player
    }

  }

  sealed trait PointsAnalysis
  case class Same(points: Points) extends PointsAnalysis
  case class Else(points: Points) extends PointsAnalysis
  object Deuce extends PointsAnalysis
  case class Advantage(player: String) extends PointsAnalysis
  case class Win(player: String) extends PointsAnalysis

  val pointToScore: Int => String = {
    case 0 => "Love"
    case 1 => "Fifteen"
    case 2 => "Thirty"
    case 3 => "Forty"
  }

  def points: (Int, Int) = (p1, p2)

  trait Points {
    def map[A](f: (Int, Int) => A): A
    def are(p: (Int, Int) => Boolean): Boolean
    def lestThenFourButNotDeuce: Boolean
    def areSame: Boolean
    def differenceIs(i: Int): Boolean
  }

  implicit class TuplePoints(t: (Int, Int)) extends Points {
    def map[A](f: (Int, Int) => A): A = f(t._1, t._2)
    def are(p: (Int, Int) => Boolean): Boolean = p(t._1, t._2)
    val lestThenFourButNotDeuce: Boolean = are((p1, p2) => p1 < 4 && p2 < 4 && !(p1 + p2 == 6))
    val areSame: Boolean = are(_ == _)
    def differenceIs(i: Int): Boolean = (p1 - p2) * (p1 - p2) == i
  }

  def wonPoint(playerName: String): Unit =
    if (playerName == "player1")
      this.p1 += 1
    else
      this.p2 += 1
}
