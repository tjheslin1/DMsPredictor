package unit

import io.github.tjheslin1.model._
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

class DiceSpec extends WordSpec with Matchers {

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
}
