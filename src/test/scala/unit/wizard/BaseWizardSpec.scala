package unit.wizard

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.wizard.BaseWizard._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.ShieldBuffCondition

class BaseWizardSpec extends UnitSpecBase {

  "calculateHealth" should {
    "calculate health for a level one cleric with average Constitution" in new TestContext {
      calculateHealth(LevelOne, 10) shouldBe 6
    }

    "calculate health for a level one cleric with high Constitution" in new TestContext {
      calculateHealth(LevelOne, 14) shouldBe 8
    }

    "calculate health for a level one cleric with low Constitution" in new TestContext {
      calculateHealth(LevelOne, 6) shouldBe 4
    }
    "calculate health for a level two cleric with average Constitution" in new TestContext {
      calculateHealth(LevelTwo, 10) shouldBe 10
    }
  }

  "armourClass" should {
    "calculate unarmoured" in new TestContext {
      calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12),
                           mageArmourPrepared = false,
                           conditions = List.empty) shouldBe 11
    }

    "calculate whilst Mage Armour is active" in new TestContext {
      calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12),
                           mageArmourPrepared = true,
                           conditions = List.empty) shouldBe 14
    }

    "calculate unarmoured with Shield spell active" in new TestContext {
      calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12),
                           mageArmourPrepared = false,
                           conditions = List(ShieldBuffCondition)) shouldBe 16
    }

    "calculate whilst Mage Armour is active and Shield spell active" in new TestContext {
      calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12),
                           mageArmourPrepared = true,
                           conditions = List(ShieldBuffCondition)) shouldBe 19
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
