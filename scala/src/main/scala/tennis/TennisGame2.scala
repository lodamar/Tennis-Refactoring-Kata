package tennis

class TennisGame2(val player1Name: String, val player2Name: String) extends TennisGame {

  var P1point = 0
  var P2point = 0

  def calculateScore(): String = {
    if (P1point >= 4 && P2point >= 0 && (P1point - P2point) >= 2) {
      return "Win for player1"
    }
    if (P2point >= 4 && P1point >= 0 && (P2point - P1point) >= 2) {
      return "Win for player2"
    }

    if (P1point > P2point && P2point >= 3) {
      return "Advantage player1"
    }
    if (P2point > P1point && P1point >= 3) {
      return "Advantage player2"
    }

    if (P1point == P2point) {
      return P1point match {
        case 0 => "Love-All"
        case 1 => "Fifteen-All"
        case 2 => "Thirty-All"
        case _ => "Deuce"
      }
    }

    if (P1point > 0 && P2point == 0) {
      return singleScoreForPoints(P1point) + "-Love"
    }
    if (P2point > 0 && P1point == 0) {
      return "Love-" + singleScoreForPoints(P2point)
    }

    if (P1point > P2point && P1point < 4) {
      val t = (P1point, P2point) match {
        case (2, 1) => ("Thirty", "Fifteen")
        case (3, 1) => ("Forty", "Fifteen")
        case (3, 2) => ("Forty", "Thirty")
      }
      return t._1 + "-" + t._2
    }
    if (P2point > P1point && P2point < 4) {
      val t = (P1point, P2point) match {
        case (1, 2) => ("Fifteen", "Thirty")
        case (1, 3) => ("Fifteen", "Forty")
        case (2, 3) => ("Thirty", "Forty")
      }
      return t._1 + "-" + t._2
    }

    throw new AssertionError("should never come here")
  }

  private def singleScoreForPoints(points: Int) = points match {
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
