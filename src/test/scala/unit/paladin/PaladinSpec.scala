package unit.paladin

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.PaladinSpells.BlessCondition
import util.TestData._

class PaladinSpec extends UnitSpecBase {

  "updateHealth" should {

    "set the Paladin to dead if the damage brings health below negative max health" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      val paladin = random[Paladin]
        .withHealth(50)
        .withMaxHealth(50)

      val updatedPaladin = paladin.updateHealth(110, Bludgeoning, Hit).asInstanceOf[Paladin]

      updatedPaladin.isAlive shouldBe false
    }

    "not handle concentration if damage taken was 0" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(1)

      val trackedConcentrationSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), concentration = true)

      val concentratingPaladin = random[Paladin]
        .withSpellKnown(trackedConcentrationSpell)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withConcentratingOn(trackedConcentrationSpell)
        .withLevel(LevelTwo)
        .withConstitution(2)

      val updatedPaladin = concentratingPaladin.updateHealth(0, Bludgeoning, Hit).asInstanceOf[Paladin]

      updatedPaladin.concentratingSpell shouldBe trackedConcentrationSpell.some
    }

    "handle loss of concentration if Paladin goes unconscious" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(19)

      val trackedConcentrationSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), concentration = true)

      val concentratingPaladin = random[Paladin]
        .withSpellKnown(trackedConcentrationSpell)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withConcentratingOn(trackedConcentrationSpell)
        .withLevel(LevelTwo)
        .withConstitution(2)
        .withHealth(1)
        .withMaxHealth(50)

      val updatedPaladin = concentratingPaladin.updateHealth(1, Bludgeoning, Hit).asInstanceOf[Paladin]

      updatedPaladin.concentratingSpell shouldBe none[Spell]
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
