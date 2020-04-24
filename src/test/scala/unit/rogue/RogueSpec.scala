package unit.rogue

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class RogueSpec extends UnitSpecBase {

  "updateHealth" should {
    "set the Rogue to dead if the damage brings health below negative max health" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      val rogue = random[Rogue]
        .withHealth(50)
        .withMaxHealth(50)

      val updatedRogue = rogue.updateHealth(110, Bludgeoning, Hit).asInstanceOf[Rogue]

      updatedRogue.isAlive shouldBe false
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
