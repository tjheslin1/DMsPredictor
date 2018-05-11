package unit

import io.github.tjheslin1.model._
import org.scalatest.{Matchers, WordSpec}

class DiceSpec extends WordSpec with Matchers {

  "roll" should {
    "return a value between the minimum and maximum values possible for a single roll" in {
      val res = D4.result(1)
      res should (be >= 1 and be <= D4.sides)
    }

    "return a value between the minimum and maximum values possible for multiple rolls" in {
      (1 to 100).foreach { i =>
        val res = D4.result(i)
        res should (be >= i and be <= i * D4.sides)
      }
    }

    "combine the result of multiple die" in {
      val d4Res = D4.result(1)
      val d6Res = D6.result(1)

      d4Res + d6Res should (be >= 2 and be <=10)
    }
  }
}
