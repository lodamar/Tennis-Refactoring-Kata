package tennis

import org.scalatest.matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec

class TennisTest extends AnyPropSpec with TableDrivenPropertyChecks with must.Matchers {

  def checkScores(game: TennisGame, testCase: TennisTestCase) {
    val highestScore = Math.max(testCase.player1Score, testCase.player2Score)
    for (i <- 0 until highestScore by 1) {
      if (i < testCase.player1Score)
        game.wonPoint("player1")
      if (i < testCase.player2Score)
        game.wonPoint("player2")
    }
    testCase.expectedScore mustBe game.calculateScore
  }

  property("check game 1") {
    forAll(examples) { set =>
      val game = new TennisGame1("player1", "player2")
      checkScores(game, set)
    }
  }

  property("check game 2") {
    forAll(examples) { set =>
      val game = new TennisGame2("player1", "player2")
      checkScores(game, set)
    }
  }

  property("check game 3") {
    forAll(examples) { set =>
      val game = new TennisGame3("player1", "player2")
      checkScores(game, set)
    }
  }

  val examples =
    Table(
      "case",
      new TennisTestCase(0, 0, "Love-All"),
      new TennisTestCase(1, 1, "Fifteen-All"),
      new TennisTestCase(2, 2, "Thirty-All"),
      new TennisTestCase(3, 3, "Deuce"),
      new TennisTestCase(4, 4, "Deuce"),
      new TennisTestCase(1, 0, "Fifteen-Love"),
      new TennisTestCase(0, 1, "Love-Fifteen"),
      new TennisTestCase(2, 0, "Thirty-Love"),
      new TennisTestCase(0, 2, "Love-Thirty"),
      new TennisTestCase(3, 0, "Forty-Love"),
      new TennisTestCase(0, 3, "Love-Forty"),
      new TennisTestCase(4, 0, "Win for player1"),
      new TennisTestCase(0, 4, "Win for player2"),
      new TennisTestCase(2, 1, "Thirty-Fifteen"),
      new TennisTestCase(1, 2, "Fifteen-Thirty"),
      new TennisTestCase(3, 1, "Forty-Fifteen"),
      new TennisTestCase(1, 3, "Fifteen-Forty"),
      new TennisTestCase(4, 1, "Win for player1"),
      new TennisTestCase(1, 4, "Win for player2"),
      new TennisTestCase(3, 2, "Forty-Thirty"),
      new TennisTestCase(2, 3, "Thirty-Forty"),
      new TennisTestCase(4, 2, "Win for player1"),
      new TennisTestCase(2, 4, "Win for player2"),
      new TennisTestCase(4, 3, "Advantage player1"),
      new TennisTestCase(3, 4, "Advantage player2"),
      new TennisTestCase(5, 4, "Advantage player1"),
      new TennisTestCase(4, 5, "Advantage player2"),
      new TennisTestCase(15, 14, "Advantage player1"),
      new TennisTestCase(14, 15, "Advantage player2"),
      new TennisTestCase(6, 4, "Win for player1"),
      new TennisTestCase(4, 6, "Win for player2"),
      new TennisTestCase(16, 14, "Win for player1"),
      new TennisTestCase(14, 16, "Win for player2")
    )

}
