package unit.barbarian

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.barbarian.Berserker
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class BerserkerSpec extends UnitSpecBase {

  "updateHealth" should {
    "set the Berserker to dead if the damage brings health below negative max health" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val barbarian = random[Berserker]
        .withHealth(50)
        .withMaxHealth(50)

      val updatedBarbarian = barbarian.updateHealth(110, Bludgeoning, Hit).asInstanceOf[Berserker]

      updatedBarbarian.isAlive shouldBe false
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }

}
