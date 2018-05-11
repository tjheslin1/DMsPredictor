package unit

import io.github.tjheslin1.model.D4
import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalatest.{Matchers, WordSpec}

class DiceSpec extends WordSpec with Matchers {

  "roll" should {
    "return a value between the minimum and maximum values possible for a roll" in {

//      val rolls = Gen.choose(0, 100)

//      forAll(rolls) { r: Int =>
//        val res = D4.result(r)
//        println(res)
//        res == 1// && res <= r*D4.sides
//      }
    }
  }
}
