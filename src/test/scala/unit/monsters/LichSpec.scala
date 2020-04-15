package unit.monsters

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.ShieldBuffCondition
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import util.TestData._

class LichSpec extends UnitSpecBase {

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
}
