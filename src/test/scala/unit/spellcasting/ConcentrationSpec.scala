package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.Concentration._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.PaladinSpells.BlessCondition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells.HuntersMarkBuffCondition
import util.TestData._

class ConcentrationSpec extends UnitSpecBase {

  "concentrationDifficultyClass" should {
    "set the DC to 10 if half the damage taken is less than 10" in {
      concentrationDifficultyClass(5) shouldBe 10
    }

    "set the DC to half the damage taken if more than 10" in {
      concentrationDifficultyClass(22) shouldBe 11
    }
  }

  "handleConcentration" should {
    "break concentration if check failed" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(8)

          val trackedSpell = trackedConditionSpell(1)

          val lowConstitutionCleric = cleric
            .withConcentratingOn(trackedSpell)
            .withConstitution(5)
            .asInstanceOf[SpellCaster]

          val updatedCleric = handleConcentration(lowConstitutionCleric,
                                                  damageTaken = 20,
                                                  concentrationSpell = trackedSpell)

          updatedCleric.isConcentrating shouldBe false
        }
      }
    }

    "maintain concentration if check passed" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(8)

          val trackedSpell = trackedConditionSpell(1)

          val highConstitutionCleric = cleric
            .withConcentratingOn(trackedSpell)
            .withConstitution(18)
            .asInstanceOf[SpellCaster]

          val updatedCleric = handleConcentration(highConstitutionCleric,
                                                  damageTaken = 10,
                                                  concentrationSpell = trackedSpell)

          updatedCleric.isConcentrating shouldBe true
        }
      }
    }

    "handle loss of concentration of ConditionSpell" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell =
            trackedConditionSpell(1, concentration = true)

          val concentratingRanger = ranger
            .withSpellKnown(trackedSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withConstitution(2)
            .withCondition(HuntersMarkBuffCondition)
            .asInstanceOf[Ranger]

          val updatedRanger =
            handleConcentration(concentratingRanger, trackedSpell, 50).asInstanceOf[Ranger]

          updatedRanger.conditions shouldBe List.empty[Condition]
          updatedRanger.concentratingSpell shouldBe none[Spell]
        }
      }
    }

    "handle loss of concentration of SelfBuffSpell" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell =
            trackedSelfBuffSpell(HuntersMarkBuffCondition, 1, concentration = true)

          val concentratingRanger = ranger
            .withSpellKnown(trackedSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withConstitution(2)
            .withCondition(HuntersMarkBuffCondition)
            .asInstanceOf[Ranger]

          val updatedRanger =
            handleConcentration(concentratingRanger, trackedSpell, 50).asInstanceOf[Ranger]

          updatedRanger.conditions shouldBe List.empty[Condition]
          updatedRanger.concentratingSpell shouldBe none[Spell]
        }
      }
    }

    "handle loss of concentration of MultiTargetBuffSpell" in {
      forAll { paladin: Paladin =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

          val concentratingPaladin = paladin
            .withSpellKnown(trackedSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withConstitution(2)
            .withCondition(BlessCondition())
            .asInstanceOf[Paladin]

          val updatedPaladin =
            handleConcentration(concentratingPaladin, trackedSpell, 50).asInstanceOf[Paladin]

          updatedPaladin.conditions shouldBe List.empty[Condition]
          updatedPaladin.concentratingSpell shouldBe none[Spell]
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
