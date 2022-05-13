package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.classes.ranger.Hunter
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastSingleTargetHealingSpell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.MagicMissile
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class CastSingleTargetHealingSpellSpec extends UnitSpecBase {

  val Priority = 1

  "castSingleTargetHealingSpell" should {
    "trigger when a players health is below 50%" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(10)

      val healingCleric    = random[Cleric].withWisdom(12).withCombatIndex(1)
      val damagedFighter   = random[Fighter].withHealth(25).withMaxHealth(100).withCombatIndex(2)
      val healthyBarbarian = random[Barbarian].withHealth(100).withMaxHealth(100).withCombatIndex(3)
      val goblin           = random[Goblin].withCombatIndex(4)

      castSingleTargetHealingSpell(Priority)(healingCleric)
        .triggerMet(List(damagedFighter, healthyBarbarian, goblin)) shouldBe true
    }

    "trigger when a players health is 0" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(10)

      val healingCleric  = random[Cleric].withWisdom(12).withCombatIndex(1)
      val damagedFighter = random[Fighter].withHealth(80).withMaxHealth(100).withCombatIndex(2)
      val unconsciousBarbarian = random[Barbarian]
        .withHealth(0)
        .withMaxHealth(100)
        .withCombatIndex(3)
      val goblin = random[Goblin].withCombatIndex(4)

      castSingleTargetHealingSpell(Priority)(healingCleric)
        .triggerMet(List(damagedFighter, unconsciousBarbarian, goblin)) shouldBe true
    }

    "not trigger when no players health are below 50%" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(10)

      val healingCleric    = random[Cleric].withWisdom(12).withCombatIndex(1)
      val healthyFighter   = random[Fighter].withHealth(90).withMaxHealth(100).withCombatIndex(2)
      val healthyBarbarian = random[Barbarian].withHealth(100).withMaxHealth(100).withCombatIndex(3)
      val goblin           = random[Goblin].withHealth(10).withMaxHealth(50).withCombatIndex(4)

      castSingleTargetHealingSpell(Priority)(healingCleric)
        .triggerMet(List(healthyFighter, healthyBarbarian, goblin)) shouldBe false
    }

    "cast a spell (healing) using the highest available spell slot" in {
      forAll { (cleric: Cleric, fighter: Fighter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell       = trackedHealingSpell(1, higherSpellSlot = true)
          val trackedAttackSpell = trackedMeleeSpellAttack(1)

          val healingCleric = cleric
            .withSpellsKnown(trackedSpell, trackedAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withWisdom(12)
            .withCombatIndex(1)

          val damagedFighter = fighter.withHealth(10).withMaxHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, healedFighter: Fighter))) = castSingleTargetHealingSpell(
            Priority)(healingCleric)
            .useAbility(List(damagedFighter), LowestFirst)

          trackedHealingSpellUsedCount shouldBe 1
          trackedHealingSpellLevelUsed shouldBe 3

          meleeSpellUsedCount shouldBe 0

          healedFighter.creature.health shouldBe 11
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (cleric: Cleric, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedHealSpell = trackedHealingSpell(1)
          val trackedSingleTargetSpell = trackedSingleTargetSavingThrowSpell(
            2,
            Dexterity,
            higherSpellSlot = false)

          val healingCleric = cleric
            .withSpellsKnown(trackedHealSpell, trackedSingleTargetSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withWisdom(12)
            .asInstanceOf[Cleric]

          val woundedHunter = hunter
            .withHealth(1)
            .withMaxHealth(100)
            .withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) = castSingleTargetHealingSpell(Priority)(
            healingCleric.withCombatIndex(1))
            .useAbility(List(woundedHunter), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe healingCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe healingCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe healingCleric.spellSlots.thirdLevel.count - 1
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (cleric: Cleric, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric
            .withSpellsKnown(trackedHealingSpell(1, higherSpellSlot = false))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withWisdom(12)
            .asInstanceOf[Cleric]

          val woundedHunter = hunter
            .withHealth(1)
            .withMaxHealth(100)
            .withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) = castSingleTargetHealingSpell(Priority)(
            healingCleric.withCombatIndex(1))
            .useAbility(List(woundedHunter), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe healingCleric.spellSlots.firstLevel.count - 1
          updatedCleric.spellSlots.secondLevel.count shouldBe healingCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe healingCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "not spend a spell slot if cantrip was found and used" in {
      forAll { (cleric: Cleric, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric
            .withSpellsKnown(trackedHealingSpell(0), trackedMeleeSpellAttack(1))
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withWisdom(12)
            .asInstanceOf[Cleric]

          val woundedHunter = hunter
            .withHealth(1)
            .withMaxHealth(100)
            .withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) = castSingleTargetHealingSpell(Priority)(
            healingCleric.withCombatIndex(1))
            .useAbility(List(woundedHunter), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe healingCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe healingCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe healingCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "not meet the condition if the Spell Caster has no healing spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withSpellKnown(MagicMissile).withCombatIndex(1)

      castSingleTargetHealingSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withNoSpellSlotsAvailable().withCombatIndex(1)

      castSingleTargetHealingSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster cannot cast any healing spells at its level" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val wizardCombatant = wizard
            .withSpellsKnown(trackedSingleTargetSavingThrowSpell(2, Wisdom), trackedHealingSpell(3))
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .withCombatIndex(1)

          castSingleTargetHealingSpell(Priority)(wizardCombatant).conditionMet shouldBe false
        }
      }
    }

    "target another Monster if caster is a Monster" in {
      forAll { (lich: Lich, fighter: Fighter, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell: Spell = trackedHealingSpell(2)

          val lichCombatant = lich
            .withSpellKnown(trackedSpell)
            .withIntelligence(10)
            .withCombatIndex(1)

          val fighterCombatant = fighter.withCombatIndex(2)

          val damagedGoblin = goblin
            .withHealth(50)
            .withMaxHealth(100)
            .withCombatIndex(3)

          val (_, List(_, Combatant(_, updatedGoblin: Goblin))) = castSingleTargetHealingSpell(
            Priority)(lichCombatant)
            .useAbility(List(fighterCombatant, damagedGoblin), LowestFirst)

          trackedHealingSpellUsedCount shouldBe 1
          trackedHealingSpellLevelUsed shouldBe 9

          updatedGoblin.creature.health shouldBe 50 + 1
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }

}
