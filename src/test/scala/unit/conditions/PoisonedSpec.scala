package unit.conditions

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Poisoned

class PoisonedSpec extends UnitSpecBase {

  "onConditionApplied" should {
    "set the creatures Attack status to Disadvantage" in {
      new TestContext {
        override implicit val roll: RollStrategy = _ => RollResult(10)

        val rogue = random[Rogue]
        val poisoned = Poisoned(10)

        val poisonedRogue = poisoned.onConditionApplied(rogue)

        poisonedRogue.attackStatus shouldBe Disadvantage
      }
    }
  }

  private abstract class TestContext {
    implicit val roll: RollStrategy
  }
}
