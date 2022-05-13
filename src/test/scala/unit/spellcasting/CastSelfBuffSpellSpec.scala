package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastSelfBuffSpell.castSelfBuffSpell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.PaladinSpells.BlessCondition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells.HuntersMarkBuffCondition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.{
  FireBolt,
  Fireball
}
import io.github.tjheslin1.dmspredictor.model.spellcasting.{Spell, SpellSlots}
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class CastSelfBuffSpellSpec extends UnitSpecBase {

  val Priority = 1

  "castSelfBuffSpell" should {
    "cast a spell (Self Buff) updating the casters conditions" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 1)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            2,
            BlessCondition(),
            higherSpellSlot = false)

          val rangerCombatant = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val (Combatant(_, updatedRanger: Ranger), _) = castSelfBuffSpell(Priority)(
            rangerCombatant)
            .useAbility(List.empty[Combatant], LowestFirst)

          selfBuffSpellUsedCount shouldBe 1
          selfBuffSpellLevelUsed shouldBe 1

          trackedMultiTargetBuffSpellUsedCount shouldBe 0
        }
      }
    }

    "set the spellCasters concentration to the cast spell if a concentration spell" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            1,
            concentration = true)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            2,
            BlessCondition(),
            higherSpellSlot = false)

          val rangerCombatant = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val (Combatant(_, updatedRanger: Ranger), _) = castSelfBuffSpell(Priority)(
            rangerCombatant)
            .useAbility(List.empty[Combatant], LowestFirst)

          selfBuffSpellUsedCount shouldBe 1
          selfBuffSpellLevelUsed shouldBe 1

          trackedMultiTargetBuffSpellUsedCount shouldBe 0

          updatedRanger.concentratingSpell shouldBe trackedBuffSpell.some
        }
      }
    }

    "not set the spellCasters concentration to the cast spell if not a concentration spell" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            1,
            concentration = false)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            2,
            BlessCondition(),
            higherSpellSlot = false)

          val rangerCombatant = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val (Combatant(_, updatedRanger: Ranger), _) = castSelfBuffSpell(Priority)(
            rangerCombatant)
            .useAbility(List.empty[Combatant], LowestFirst)

          selfBuffSpellUsedCount shouldBe 1
          selfBuffSpellLevelUsed shouldBe 1

          trackedMultiTargetBuffSpellUsedCount shouldBe 0

          updatedRanger.concentratingSpell shouldBe none[Spell]
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            1,
            higherSpellSlot = true)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            2,
            BlessCondition(),
            higherSpellSlot = false)

          val buffingWizard = wizard
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = buffingWizard.withCombatIndex(1)

          val (Combatant(_, updatedWizard: Wizard), _) = castSelfBuffSpell(Priority)(
            wizardCombatant)
            .useAbility(List.empty[Combatant], LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe buffingWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe buffingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe buffingWizard.spellSlots.thirdLevel.count - 1
        }
      }
    }

    "spend the highest available spell slot for a concentration spell" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            1,
            concentration = true,
            higherSpellSlot = true)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            2,
            BlessCondition(),
            higherSpellSlot = false)

          val buffingWizard = wizard
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = buffingWizard.withCombatIndex(1)

          val (Combatant(_, updatedWizard: Wizard), _) = castSelfBuffSpell(Priority)(
            wizardCombatant)
            .useAbility(List.empty[Combatant], LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe buffingWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe buffingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe buffingWizard.spellSlots.thirdLevel.count - 1
        }
      }
    }

    "spend the lowest available spell slot when using a higher slot has no benefit" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            1,
            higherSpellSlot = false)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            2,
            BlessCondition(),
            higherSpellSlot = false)

          val buffingWizard = wizard
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = buffingWizard.withCombatIndex(1)

          val (Combatant(_, updatedWizard: Wizard), _) = castSelfBuffSpell(Priority)(
            wizardCombatant)
            .useAbility(List.empty[Combatant], LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe buffingWizard.spellSlots.firstLevel.count - 1
          updatedWizard.spellSlots.secondLevel.count shouldBe buffingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe buffingWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "spend the lowest available spell slot when using a higher slot has no benefit for a concentration spell" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            1,
            concentration = true,
            higherSpellSlot = false)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            2,
            BlessCondition(),
            higherSpellSlot = false)

          val buffingWizard = wizard
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = buffingWizard.withCombatIndex(1)

          val (Combatant(_, updatedWizard: Wizard), _) = castSelfBuffSpell(Priority)(
            wizardCombatant)
            .useAbility(List.empty[Combatant], LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe buffingWizard.spellSlots.firstLevel.count - 1
          updatedWizard.spellSlots.secondLevel.count shouldBe buffingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe buffingWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "meet the condition if the Spell Caster has a Self Buff spell to cast" in new TestContext {
      implicit val roll: RollStrategy = Dice.defaultRandomiser

      val wizard = random[Wizard]
        .withSpellsKnown(trackedSelfBuffSpell(HuntersMarkBuffCondition, 2))
        .withAllSpellSlotsAvailableForLevel(LevelFour)
        .withLevel(LevelFour)
        .withCombatIndex(1)

      castSelfBuffSpell(Priority)(wizard).conditionMet shouldBe true
    }

    "meet the condition if the Spell Caster has only a Self Buff cantrip to cast" in new TestContext {
      implicit val roll: RollStrategy = Dice.defaultRandomiser

      val wizard = random[Wizard]
        .withSpellsKnown(trackedSelfBuffSpell(HuntersMarkBuffCondition, 0))
        .withCombatIndex(1)

      castSelfBuffSpell(Priority)(wizard).conditionMet shouldBe true
    }

    "not meet the condition if the Spell Caster does not have a Self Buff spell to cast" in new TestContext {
      implicit val roll: RollStrategy = Dice.defaultRandomiser

      val wizard = random[Wizard]
        .withSpellsKnown(FireBolt, Fireball)
        .withCombatIndex(1)

      castSelfBuffSpell(Priority)(wizard).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster cannot cast any Self Buff spells at its level" in new TestContext {
      implicit val roll: RollStrategy = Dice.defaultRandomiser

      val wizard = random[Wizard]
        .withSpellsKnown(FireBolt, trackedSelfBuffSpell(HuntersMarkBuffCondition, 3))
        .withSpellSlots(SpellSlots(firstLevelSlots = 4, secondLevelSlots = 3, thirdLevelSlots = 0))
        .withLevel(LevelFive)
        .withCombatIndex(1)

      castSelfBuffSpell(Priority)(wizard).conditionMet shouldBe false
    }

    "cast a spell (Self Buff) for a Monster updating the casters conditions" in {
      forAll { lich: Lich =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 1)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            2,
            BlessCondition(),
            higherSpellSlot = false)

          val buffingLich = lich
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withCombatIndex(1)

          val (Combatant(_, updatedLich: Lich), _) = castSelfBuffSpell(Priority)(buffingLich)
            .useAbility(List.empty[Combatant], LowestFirst)

          selfBuffSpellUsedCount shouldBe 1
          selfBuffSpellLevelUsed shouldBe 9

          trackedMultiTargetBuffSpellUsedCount shouldBe 0
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
