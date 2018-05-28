package unit

import io.github.tjheslin1.model._
import org.scalatest.{Matchers, WordSpec}

class DiceSpec extends WordSpec with Matchers {

  "roll" should {
    "return the minimum value for minimum rolls" in {
      val res = D4.roll(1, _ => 1)
      res shouldBe 1
    }

    "return the maximum value for maximum rolls" in {
      val res = D4.roll(1, _ => 4)
      res shouldBe 4
    }

    "combine the result of multiple die" in {
      val d4Res = D4.roll(1, _ => 2)
      val d6Res = D6.roll(1, _ => 3)

      d4Res + d6Res shouldBe 5
    }
  }
}
