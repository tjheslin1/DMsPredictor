package unit.fighter

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class FighterSpec extends UnitSpecBase {

  "updateHealth" should {
    "set the Fighter to dead if the damage brings health below negative max health" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      val fighter = random[Fighter]
        .withHealth(50)
        .withMaxHealth(50)

      val updatedFighter = fighter.updateHealth(110, Bludgeoning, Hit).asInstanceOf[Fighter]

      updatedFighter.isAlive shouldBe false
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
