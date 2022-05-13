package tennis

class TennisGame2(val player1Name: String, val player2Name: String) extends TennisGame {

  var P1point = 0
  var P2point = 0

  def calculateScore(): String = {
    val analysis: ScoreAnalyzer = (P1point, P2point) match {
      case (p1, p2) if p1 >= 4 && p2 >= 0 && (p1 - p2) >= 2 => Win1
      case (p1, p2) if p2 >= 4 && p1 >= 0 && (p2 - p1) >= 2 => Win2
      case (p1, p2) if p1 > p2 && p2 >= 3                   => Advantage1
      case (p1, p2) if p2 > p1 && p1 >= 3                   => Advantage2
      case (p1, p2) if p1 == p2                             => Same(p1)
      case (p1, p2) if p1 > 0 && p2 == 0                    => Love2(p1)
      case (p1, p2) if p2 > 0 && p1 == 0                    => Love1(p2)
      case (p1, p2)                                         => Else(p1, p2)
    }

    analysis match {
      case Win1         => "Win for player1"
      case Win2         => "Win for player2"
      case Advantage1   => "Advantage player1"
      case Advantage2   => "Advantage player2"
      case Same(p)      => pointsToScore(p, p)
      case Love2(p1)    => pointsToScore(p1, 0)
      case Love1(p2)    => pointsToScore(0, p2)
      case Else(p1, p2) => pointsToScore(p1, p2)
    }
  }

  private def pointsToScore(p1: Int, p2: Int): String =
    (p1, p2) match {
      case (0, 0)                     => "Love-All"
      case (a, b) if a == b && a <= 2 => pointsToScore(a) + "-All"
      case (a, b) if a == b           => "Deuce"
      case _                          => pointsToScore(p1) + "-" + pointsToScore(p2)
    }

  private def pointsToScore(points: Int) = points match {
    case 0 => "Love"
    case 1 => "Fifteen"
    case 2 => "Thirty"
    case 3 => "Forty"
  }

  def wonPoint(player: String): Unit =
    if (player == "player1")
      P1point += 1
    else
      P2point += 1

}

trait ScoreAnalyzer
case class Else(p1: Int, p2: Int) extends ScoreAnalyzer
case class Same(p: Int) extends ScoreAnalyzer
case class Love2(p: Int) extends ScoreAnalyzer
case class Love1(p: Int) extends ScoreAnalyzer
object Win1 extends ScoreAnalyzer
object Win2 extends ScoreAnalyzer
object Advantage1 extends ScoreAnalyzer
object Advantage2 extends ScoreAnalyzer
