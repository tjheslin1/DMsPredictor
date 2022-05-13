package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model._

import scala.util.Random

class DiceSpec extends UnitSpecBase {

  implicit def rollResultConversion(roll: Int): RollResult = RollResult(roll)

  "defaultRandomiser" should {
    "return an inclusive value between the minimum and maximum value of the die" in {
      (1 to 1000).map(_ => {
        val res = D6.roll()(_ => Random.nextInt(D6.sides) + 1)

        res should (be >= 1 and be <= 6)
      })
    }
  }

  "roll" should {
    "return the minimum value for minimum rolls" in {
      val res = D4.roll()(_ => 1)
      res shouldBe 1
    }

    "return the maximum value for maximum rolls" in {
      val res = D4.roll()(_ => 4)
      res shouldBe 4
    }

    "combine the result of multiple die" in {
      val d4Res = D4.roll()(_ => 2)
      val d6Res = D6.roll()(_ => 3)

      d4Res + d6Res shouldBe 5
    }
  }

  "rollWithAdvantage" should {

    "take the highest value of two rolls" in {
      val iterator = Iterator(5, 15)
      val res      = D20.rollWithAdvantage()(_ => iterator.next())

      res shouldBe 15
    }

    "take the highest value over multiple rolls" in {
      val iterator = Iterator(5, 15, 19, 4)
      val res      = D20.rollWithAdvantage(2)(_ => iterator.next())

      res shouldBe 15 + 19
    }
  }

  "rollWithDisadvantage" should {

    "take the lowest value of two rolls" in {
      val iterator = Iterator(5, 15)
      val res      = D20.rollWithDisadvantage()(_ => iterator.next())

      res shouldBe 5
    }

    "take the highest value over multiple rolls" in {
      val iterator = Iterator(5, 15, 19, 4)
      val res      = D20.rollWithDisadvantage(2)(_ => iterator.next())

      res shouldBe 5 + 4
    }
  }

  "midpointRoundedUp" should {

    "return 3 for d4" in {
      Dice.midpointRoundedUp(D4) shouldBe 3
    }

    "return 4 for d6" in {
      Dice.midpointRoundedUp(D6) shouldBe 4
    }

    "return 5 for d8" in {
      Dice.midpointRoundedUp(D8) shouldBe 5
    }

    "return 6 for d10" in {
      Dice.midpointRoundedUp(D10) shouldBe 6
    }

    "return 7 for d12" in {
      Dice.midpointRoundedUp(D12) shouldBe 7
    }

    "return 11 for d20" in {
      Dice.midpointRoundedUp(D20) shouldBe 11
    }

    "return 51 for d100" in {
      Dice.midpointRoundedUp(D100) shouldBe 51
    }
  }
}
