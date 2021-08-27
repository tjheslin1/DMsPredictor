package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.barbarian.Berserker
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Champion
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.classes.ranger.Hunter
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastMultiTargetBuffSpell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.PaladinSpells.BlessCondition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells.HuntersMarkBuffCondition
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class CastMultiTargetBuffSpellSpec extends UnitSpecBase {

  val Priority = 1

  "castMultiTargetBuffSpell" should {
    "cast a spell (Multi Target Buff) updating the targets conditions" in {
      forAll { (paladin: Paladin, hunter: Hunter, berserker: Berserker) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffTwoTargetsSpell = trackedMultiTargetBuffSpell(
            1,
            BlessCondition(),
            numTargets = 2)
          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            2,
            higherSpellSlot = false)

          val castingPaladin = paladin
            .withSpellsKnown(trackedBuffTwoTargetsSpell, trackedBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val hunterCombatant = hunter.withCombatIndex(2)

          val berserkerCombatant = berserker.withCombatIndex(3)

          val (
            Combatant(_, updatedPaladin: Paladin),
            List(Combatant(_, updatedHunter: Hunter), Combatant(_, updatedBerserker: Berserker))) =
            castMultiTargetBuffSpell(Priority)(castingPaladin)
              .useAbility(List(hunterCombatant, berserkerCombatant), LowestFirst)

          trackedMultiTargetBuffSpellUsedCount shouldBe 1
          trackedMultiTargetBuffSpellLevelUsed shouldBe 1

          selfBuffSpellUsedCount shouldBe 0

          updatedPaladin.conditions shouldBe List.empty[Condition]

          updatedHunter.conditions shouldBe List(BlessCondition())
          updatedBerserker.conditions shouldBe List(BlessCondition())
        }
      }
    }

    "cast a spell (Multi Target Buff) updating the SpellCasters own conditions" in {
      forAll { (paladin: Paladin, hunter: Hunter, berserker: Berserker) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffThreeTargetsSpell = trackedMultiTargetBuffSpell(
            1,
            BlessCondition(),
            numTargets = 3)
          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            2,
            higherSpellSlot = false)

          val castingPaladin = paladin
            .withSpellsKnown(trackedBuffThreeTargetsSpell, trackedBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val hunterCombatant = hunter.withCombatIndex(2)

          val berserkerCombatant = berserker.withCombatIndex(3)

          val (
            Combatant(_, updatedPaladin: Paladin),
            List(Combatant(_, updatedHunter: Hunter), Combatant(_, updatedBerserker: Berserker))) =
            castMultiTargetBuffSpell(Priority)(castingPaladin)
              .useAbility(List(hunterCombatant, berserkerCombatant), LowestFirst)

          trackedMultiTargetBuffSpellUsedCount shouldBe 1
          trackedMultiTargetBuffSpellLevelUsed shouldBe 1

          selfBuffSpellUsedCount shouldBe 0

          updatedPaladin.conditions shouldBe List(BlessCondition())

          updatedHunter.conditions shouldBe List(BlessCondition())
          updatedBerserker.conditions shouldBe List(BlessCondition())
        }
      }
    }

    "target Monsters if caster is a Monster" in {
      forAll { (lich: Lich, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(1, BlessCondition())
          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            2,
            higherSpellSlot = false)

          val castingLich = lich
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withCombatIndex(1)

          val goblinCombatant = goblin.withCombatIndex(2)

          val (Combatant(_, updatedLich: Lich), List(Combatant(_, updatedGoblin: Goblin))) =
            castMultiTargetBuffSpell(Priority)(castingLich)
              .useAbility(List(goblinCombatant), LowestFirst)

          trackedMultiTargetBuffSpellUsedCount shouldBe 1
          trackedMultiTargetBuffSpellLevelUsed shouldBe 9

          selfBuffSpellUsedCount shouldBe 0

          updatedLich.conditions shouldBe List(BlessCondition())

          updatedGoblin.conditions shouldBe List(BlessCondition())
        }
      }
    }

    "set the spellCasters concentration to the cast spell if a concentration spell" in {
      forAll { (paladin: Paladin, champion: Champion) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            1,
            BlessCondition(),
            concentration = true)
          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            2,
            higherSpellSlot = false)

          val paladinCombatant = paladin
            .withSpellKnown(trackedMultiBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val championCombatant = champion.withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), _) = castMultiTargetBuffSpell(Priority)(
            paladinCombatant)
            .useAbility(List(championCombatant), LowestFirst)

          trackedMultiTargetBuffSpellUsedCount shouldBe 1
          trackedMultiTargetBuffSpellLevelUsed shouldBe 1

          selfBuffSpellUsedCount shouldBe 0

          updatedPaladin.concentratingSpell shouldBe trackedMultiBuffSpell.some
        }
      }
    }

    "not set the spellCasters concentration to the cast spell if not a concentration spell" in {
      forAll { (paladin: Paladin, champion: Champion) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            1,
            BlessCondition(),
            concentration = false)
          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            2,
            higherSpellSlot = false)

          val paladinCombatant = paladin
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val championCombatant = champion.withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), _) = castMultiTargetBuffSpell(Priority)(
            paladinCombatant)
            .useAbility(List(championCombatant), LowestFirst)

          trackedMultiTargetBuffSpellUsedCount shouldBe 1
          trackedMultiTargetBuffSpellLevelUsed shouldBe 1

          selfBuffSpellUsedCount shouldBe 0

          updatedPaladin.concentratingSpell shouldBe none[Spell]
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (wizard: Wizard, paladin: Paladin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            1,
            BlessCondition(),
            higherSpellSlot = true)
          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            2,
            higherSpellSlot = false)

          val castingWizard = wizard
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val paladinCombatant = paladin.withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) = castMultiTargetBuffSpell(Priority)(
            castingWizard.withCombatIndex(1))
            .useAbility(List(paladinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe castingWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe castingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe (castingWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the highest available spell slot for concentration spell" in {
      forAll { (wizard: Wizard, paladin: Paladin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            1,
            BlessCondition(),
            concentration = true,
            higherSpellSlot = true)
          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            2,
            higherSpellSlot = false)

          val castingWizard = wizard
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val paladinCombatant = paladin.withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) = castMultiTargetBuffSpell(Priority)(
            castingWizard.withCombatIndex(1))
            .useAbility(List(paladinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe castingWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe castingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe (castingWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot if using a higher slot has no benefit" in {
      forAll { (wizard: Wizard, paladin: Paladin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            1,
            BlessCondition(),
            higherSpellSlot = false)
          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            2,
            higherSpellSlot = false)

          val castingWizard = wizard
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val paladinCombatant = paladin.withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) = castMultiTargetBuffSpell(Priority)(
            castingWizard.withCombatIndex(1))
            .useAbility(List(paladinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe (castingWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.secondLevel.count shouldBe castingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe castingWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "spend the lowest available spell slot if using a higher slot has no benefit for a concentration spell" in {
      forAll { (wizard: Wizard, paladin: Paladin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(
            1,
            BlessCondition(),
            concentration = true,
            higherSpellSlot = false)
          val trackedBuffSpell = trackedSelfBuffSpell(
            HuntersMarkBuffCondition,
            2,
            higherSpellSlot = false)

          val castingWizard = wizard
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val paladinCombatant = paladin.withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) = castMultiTargetBuffSpell(Priority)(
            castingWizard.withCombatIndex(1))
            .useAbility(List(paladinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe (castingWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.secondLevel.count shouldBe castingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe castingWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "be triggered if the caster has at least one ally to buff" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

      val paladin = random[Paladin]
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withSpellKnown(trackedSpell)
        .withLevel(LevelTwo)
        .withCombatIndex(2)

      val rogue = random[Rogue].withCombatIndex(2)

      val goblin = random[Goblin].withCombatIndex(3)

      castMultiTargetBuffSpell(Priority)(paladin).triggerMet(List(rogue, goblin)) shouldBe true
    }

    "be triggered for a Monster if the caster has at least one ally to buff" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

      val lich = random[Lich]
        .withSpellKnown(trackedSpell)
        .withCombatIndex(2)

      val goblin = random[Goblin].withCombatIndex(2)

      val rogue = random[Rogue].withCombatIndex(3)

      castMultiTargetBuffSpell(Priority)(lich).triggerMet(List(goblin, rogue)) shouldBe true
    }

    "not be triggered if the caster does not have any allies to buff" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

      val paladin = random[Paladin]
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withSpellKnown(trackedSpell)
        .withLevel(LevelTwo)
        .withCombatIndex(2)

      val goblin = random[Goblin].withCombatIndex(2)

      castMultiTargetBuffSpell(Priority)(paladin).triggerMet(List(goblin)) shouldBe false
    }

    "not be triggered for a Monster if the caster does not have any allies to buff" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

      val lich = random[Lich]
        .withSpellKnown(trackedSpell)
        .withCombatIndex(2)

      val rogue = random[Rogue].withCombatIndex(2)

      castMultiTargetBuffSpell(Priority)(lich).triggerMet(List(rogue)) shouldBe false
    }

    "meet the condition if the Spell Caster has a Multi Target Buff cantrip to cast" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(0, BlessCondition())

      val cleric = random[Cleric]
        .withSpellKnown(trackedSpell)
        .withCombatIndex(1)

      castMultiTargetBuffSpell(Priority)(cleric).conditionMet shouldBe true
    }

    "meet the condition if the Spell Caster has a Multi Target Buff spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

      val paladin = random[Paladin]
        .withSpellKnown(trackedSpell)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withLevel(LevelTwo)
        .withCombatIndex(1)

      castMultiTargetBuffSpell(Priority)(paladin).conditionMet shouldBe true
    }

    "not meet the condition if the Spell Caster cannot cast any Multi Target Buff spells at its level" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val trackedMeleeAttackSpell = trackedMultiTargetSavingThrowSpell(1, Strength)

      val paladin = random[Paladin]
        .withSpellKnown(trackedMeleeAttackSpell)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withLevel(LevelTwo)
        .withCombatIndex(1)

      castMultiTargetBuffSpell(Priority)(paladin).conditionMet shouldBe false
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
