package unit.wizard

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells.HuntersMark
import util.TestData._

class WizardSpec extends UnitSpecBase {

  "updateHealth" should {

      "set the Wizard to dead if the damage brings health below negative max health" in new TestContext {
        override implicit val roll: RollStrategy = _ => RollResult(10)

        val wizard = random[Wizard]
          .withHealth(50)
          .withMaxHealth(50)

        val updatedWizard = wizard.updateHealth(110, Bludgeoning, Hit).asInstanceOf[Wizard]

        updatedWizard.isAlive shouldBe false
      }

    "not handle concentration if damage taken was 0" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(1)

      val concentratingWizard = random[Wizard]
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withConcentratingOn(HuntersMark)
        .withLevel(LevelTwo)
        .withConstitution(2)

      val updatedWizard = concentratingWizard.updateHealth(0, Bludgeoning, Hit).asInstanceOf[Wizard]

      updatedWizard.concentratingSpell shouldBe HuntersMark.some
    }

    "handle loss of concentration if wizard goes unconscious" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(19)

      val concentratingWizard = random[Wizard]
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withConcentratingOn(HuntersMark)
        .withLevel(LevelTwo)
        .withConstitution(2)
        .withHealth(1)
        .withMaxHealth(50)

      val updatedWizard = concentratingWizard.updateHealth(1, Bludgeoning, Hit).asInstanceOf[Wizard]

      updatedWizard.concentratingSpell shouldBe none[Spell]
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
