package tennis

class TennisGame1(val player1Name: String, val player2Name: String) extends TennisGame {
  var m_score1: Int = 0
  var m_score2: Int = 0

  def wonPoint(playerName: String) {
    if (playerName == "player1")
      m_score1 += 1
    else
      m_score2 += 1
  }

  def calculateScore(): String =
    if (m_score1 == m_score2) {
      scoreWhenSamePoints(m_score1)
    } else if (m_score1 >= 4 || m_score2 >= 4) {
      scoreAtDeuceTieBreak(m_score1 - m_score2)
    } else {
      scoreWhenDifferentPointsBeforeDeuce(m_score1, m_score2)
    }

  private def scoreWhenDifferentPointsBeforeDeuce(points1: Int, points2: Int): String =
    List(points1, points2).map(pointsToScore).mkString("-")

  private def pointsToScore(points: Int): String =
    points match {
      case 0 => "Love"
      case 1 => "Fifteen"
      case 2 => "Thirty"
      case 3 => "Forty"
    }

  private def scoreAtDeuceTieBreak(difference: Int): String =
    difference match {
      case 1           => "Advantage player1"
      case -1          => "Advantage player2"
      case n if n >= 2 => "Win for player1"
      case _           => "Win for player2"
    }

  private def scoreWhenSamePoints(points: Int): String =
    points match {
      case 0 => "Love-All"
      case 1 => "Fifteen-All"
      case 2 => "Thirty-All"
      case _ => "Deuce"
    }
}
