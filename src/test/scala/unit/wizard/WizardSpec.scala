package unit.wizard

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells.HuntersMark
import util.TestData._

class WizardSpec extends UnitSpecBase {

  "updateHealth" should {
    "not handle concentration if damage taken was 0" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(19)

      val concentratingWizard = random[Wizard]
        .withSpellKnown(HuntersMark)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withConcentratingOn(HuntersMark)
        .withLevel(LevelTwo)
        .withConstitution(2)
        .asInstanceOf[Wizard]

      val updatedWizard = concentratingWizard.updateHealth(0, Bludgeoning, Hit).asInstanceOf[Wizard]

      updatedWizard.concentratingSpell shouldBe HuntersMark.some
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
