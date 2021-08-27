package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastSingleTargetInstantEffectSpell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells.HuntersMarkBuffCondition
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class CastSingleTargetInstantEffectSpellSpec extends UnitSpecBase {

  val Priority = 1

  "castSingleTargetInstantEffectSpell" should {

    "cast a spell (Instant Effect) using the highest available spell slot" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(1, higherSpellSlot = true)

          val instantEffectWizard = wizard
            .withSpellKnown(trackedInstantSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val goblinCombatant = goblin
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          castSingleTargetInstantEffectSpell(1)(instantEffectWizard)
            .useAbility(List(goblinCombatant), LowestFirst)

          trackedInstantEffectUsedCount shouldBe 1
          trackedInstantEffectSpellLevelUsed shouldBe 3
        }
      }
    }

    "cast a spell (Instant Effect) using the lowest available spell slot necessary" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(
            1,
            setHealthToOneEffect,
            higherSpellSlot = false)

          val instantEffectWizard = wizard
            .withSpellKnown(trackedInstantSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val goblinCombatant = goblin
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) = castSingleTargetInstantEffectSpell(
            1)(instantEffectWizard).useAbility(List(goblinCombatant), LowestFirst)

          trackedInstantEffectUsedCount shouldBe 1
          trackedInstantEffectSpellLevelUsed shouldBe 1
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(
            1,
            setHealthToOneEffect,
            higherSpellSlot = true)
          val trackedSingleTargetSpell = trackedSingleTargetSavingThrowSpell(
            2,
            Dexterity,
            higherSpellSlot = false)

          val instantEffectWizard = wizard
            .withSpellsKnown(trackedInstantSpell, trackedSingleTargetSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) = castSingleTargetInstantEffectSpell(1)(
            instantEffectWizard.withCombatIndex(1))
            .useAbility(List(goblinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe instantEffectWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe instantEffectWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe (instantEffectWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(
            1,
            setHealthToOneEffect,
            higherSpellSlot = false)
          val trackedSingleTargetSpell = trackedSingleTargetSavingThrowSpell(
            2,
            Dexterity,
            higherSpellSlot = false)

          val instantEffectWizard = wizard
            .withSpellsKnown(trackedInstantSpell, trackedSingleTargetSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) = castSingleTargetInstantEffectSpell(1)(
            instantEffectWizard.withCombatIndex(1))
            .useAbility(List(goblinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe (instantEffectWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.secondLevel.count shouldBe instantEffectWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe instantEffectWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "not spend a spell slot if cantrip was found and used" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(
            0,
            setHealthToOneEffect,
            higherSpellSlot = true)

          val instantEffectWizard = wizard
            .withSpellKnown(trackedInstantSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) = castSingleTargetInstantEffectSpell(1)(
            instantEffectWizard.withCombatIndex(1))
            .useAbility(List(goblinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe instantEffectWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe instantEffectWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe instantEffectWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "meet the condition if the Spell Caster has an Instant Effect spell to cast at a level it can cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val buffSpellWizard = random[Wizard]
        .withSpellKnown(trackedInstantEffectSpell(1))
        .withCombatIndex(1)

      castSingleTargetInstantEffectSpell(1)(buffSpellWizard).conditionMet shouldBe true
    }

    "not meet the condition if the Spell Caster has no Instant Effect spells to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val buffSpellWizard = random[Wizard]
        .withSpellKnown(trackedSelfBuffSpell(HuntersMarkBuffCondition, 1))
        .withCombatIndex(1)

      castSingleTargetInstantEffectSpell(1)(buffSpellWizard).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val noSpellsWizard = random[Wizard]
        .withSpellsKnown(List.empty[Spell]: _*)
        .withCombatIndex(1)

      castSingleTargetInstantEffectSpell(1)(noSpellsWizard).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster cannot cast any Instant Effect spells at its level" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val noSpellsWizard = random[Wizard]
        .withSpellsKnown(
          trackedMeleeSpellAttack(1),
          trackedInstantEffectSpell(2, setHealthToOneEffect))
        .withAllSpellSlotsAvailableForLevel(LevelOne)
        .withLevel(LevelOne)
        .withCombatIndex(1)

      castSingleTargetInstantEffectSpell(1)(noSpellsWizard).conditionMet shouldBe false
    }

    "target Player if caster is a Monster" in {
      forAll { (lich: Lich, fighter: Fighter, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(
            1,
            setHealthToOneEffect,
            higherSpellSlot = true)

          val instantEffectWizard = lich
            .withSpellKnown(trackedInstantSpell)
            .withCombatIndex(1)

          val (
            _,
            List(Combatant(_, updatedFighter: Fighter), Combatant(_, updatedGoblin: Goblin))) =
            castSingleTargetInstantEffectSpell(1)(instantEffectWizard)
              .useAbility(List(fighter.withCombatIndex(2), goblin.withCombatIndex(3)), LowestFirst)

          trackedInstantEffectUsedCount shouldBe 1
          trackedInstantEffectSpellLevelUsed shouldBe 9

          updatedFighter.health shouldBe 1
          updatedGoblin.health shouldBe goblin.health
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
