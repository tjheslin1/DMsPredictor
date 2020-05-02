package unit.monsters

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.ShieldBuffCondition
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import util.TestData._

class LichSpec extends UnitSpecBase {

  "updateHealth" should {
    "set the Lich to dead if the damage brings health below negative max health" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      val lich = random[Lich]
        .withNoSpellSlots()
        .withHealth(50)
        .withMaxHealth(50)

      val updatedLich = lich.updateHealth(110, Radiant, Hit).asInstanceOf[Lich]

      updatedLich.isAlive shouldBe false
    }
  }

  "calculateArmourClass" should {
    "include bonus AC if Lich has the ShieldBuffCondition" in {
      val shieldBuffedLich = random[Lich].withCondition(ShieldBuffCondition())

      shieldBuffedLich.armourClass shouldBe 22
    }

    "use the Lich's base AC if the Lich does not has the ShieldBuffCondition" in {
      val shieldBuffedLich = random[Lich].withNoConditions()

      shieldBuffedLich.armourClass shouldBe 17
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
