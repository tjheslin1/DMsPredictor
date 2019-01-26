package unit.barbarian

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarianAbilities._
import io.github.tjheslin1.dmspredictor.classes.barbarian._
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class BaseBarbarianAbilitiesSpec extends UnitSpecBase {

  "rage" should {
    "update the barbarian's number of rages left" in new TestContext {
      val ragedBarbarian = random[Barbarian].withRageUsagesLeft(2).withCombatIndex(1)

      val updatedBarbarian = rage(1)(ragedBarbarian).update.asInstanceOf[Barbarian]

      updatedBarbarian.rageUsages shouldBe 1
    }

    "update the barbarian's inRage to true" in new TestContext {
      val ragedBarbarian = random[Barbarian].withRageUsagesLeft(2).withCombatIndex(1)

      val updatedBarbarian = rage(1)(ragedBarbarian).update.asInstanceOf[Barbarian]

      updatedBarbarian.inRage shouldBe true
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
