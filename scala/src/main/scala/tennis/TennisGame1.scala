package tennis

class TennisGame1(val player1Name: String, val player2Name: String) extends TennisGame {
  var points: Points = Points(player1 = player1Name, player2 = player2Name)

  def wonPoint(playerName: String): Unit =
    points = points.addOneTo(playerName)

  def calculateScore(): String =
    Score.scoreFor(points)

}

case class Points(player1: String, points1: Int = 0, player2: String, points2: Int = 0) {
  val same: Boolean = points1 == points2
  val deuceTieBreak: Boolean = points1 >= 4 || points2 >= 4

  def addOneTo(player: String): Points =
    if (player == player1) copy(points1 = points1 + 1)
    else copy(points2 = points2 + 1)
}

object Score {
  def scoreFor(points: Points): String =
    if (points.same) {
      scoreWhenSamePoints(points.points1)
    } else if (points.deuceTieBreak) {
      scoreAtDeuceTieBreak(points.points1 - points.points2)
    } else {
      scoreWhenDifferentPointsBeforeDeuce(points.points1, points.points2)
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
