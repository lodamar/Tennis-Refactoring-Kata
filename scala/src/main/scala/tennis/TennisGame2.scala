package tennis

class TennisGame2(val player1Name: String, val player2Name: String) extends TennisGame {

  var P1point = 0
  var P2point = 0

  var P1res = ""
  var P2res = ""

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

    var score = ""
    if (P1point > 0 && P2point == 0) {
      if (P1point == 1)
        P1res = "Fifteen"
      if (P1point == 2)
        P1res = "Thirty"
      if (P1point == 3)
        P1res = "Forty"

      P2res = "Love"
      score = P1res + "-" + P2res
    }
    if (P2point > 0 && P1point == 0) {
      if (P2point == 1)
        P2res = "Fifteen"
      if (P2point == 2)
        P2res = "Thirty"
      if (P2point == 3)
        P2res = "Forty"

      P1res = "Love"
      score = P1res + "-" + P2res
    }

    if (P1point > P2point && P1point < 4) {
      if (P1point == 2)
        P1res = "Thirty"
      if (P1point == 3)
        P1res = "Forty"
      if (P2point == 1)
        P2res = "Fifteen"
      if (P2point == 2)
        P2res = "Thirty"
      score = P1res + "-" + P2res
    }
    if (P2point > P1point && P2point < 4) {
      if (P2point == 2)
        P2res = "Thirty"
      if (P2point == 3)
        P2res = "Forty"
      if (P1point == 1)
        P1res = "Fifteen"
      if (P1point == 2)
        P1res = "Thirty"
      score = P1res + "-" + P2res
    }

    return score
  }

  def wonPoint(player: String): Unit = {
    if (player == "player1")
      P1point += 1
    else
      P2point += 1
  }

}
