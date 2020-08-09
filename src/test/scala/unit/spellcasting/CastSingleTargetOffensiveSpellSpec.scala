package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastSingleTargetOffensiveSpell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.CureWounds
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

class CastSingleTargetOffensiveSpellSpec extends UnitSpecBase {

  val Priority = 1

  "castSingleTargetOffensiveSpell" should {
    "cast a spell (spell attack)" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedSpell = trackedMeleeSpellAttack(1)
          val trackedMultiSpell = trackedMultiTargetSavingThrowSpell(2, Strength, higherSpellSlot = false)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSpell, trackedMultiSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(trackedCleric)
            .useAbility(List(monster), LowestFirst)

          meleeSpellUsedCount shouldBe 1
          meleeSpellLevelUsed shouldBe 3

          multiTargetSavingThrowSpellUsedCount shouldBe 0
        }
      }
    }

    "cast a spell (saving throw) using the highest available spell slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedSingleTargetSavingThrowSpell(1, Wisdom)
          val trackedMultiSpell = trackedMultiTargetSavingThrowSpell(2, Strength, higherSpellSlot = false)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSpell, trackedMultiSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withWisdom(10)
            .withCombatIndex(1)

          val monster = testMonster.withWisdom(10).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(trackedCleric)
            .useAbility(List(monster), LowestFirst)

          singleTargetSavingThrowSpellUsedCount shouldBe 1
          singleTargetSavingThrowSpellLevelUsed shouldBe 3

          multiTargetSavingThrowSpellUsedCount shouldBe 0
        }
      }
    }

    "cast a spell (saving throw) using the highest available spell slot which has a damaging spell" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedSingleTargetSavingThrowSpell(2, Strength)
          val trackedMultiSpell = trackedMultiTargetSavingThrowSpell(2, Wisdom, higherSpellSlot = false)
          val trackedHealSpell = trackedHealingSpell(3)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSpell, trackedMultiSpell, trackedHealSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withWisdom(20)
            .withCombatIndex(1)

          val goblinCombatant = goblin.withStrength(1).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(trackedCleric)
            .useAbility(List(goblinCombatant), LowestFirst)

          singleTargetSavingThrowSpellUsedCount shouldBe 1
          singleTargetSavingThrowSpellLevelUsed shouldBe 3

          multiTargetSavingThrowSpellUsedCount shouldBe 0

          trackedHealingSpellUsedCount shouldBe 0
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedSingleTargetSpell = trackedMeleeSpellAttack(1)
          val trackedMultiTargetSpell = trackedMultiTargetSavingThrowSpell(2, Strength, higherSpellSlot = false)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSingleTargetSpell, trackedMultiTargetSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castSingleTargetOffensiveSpell(Priority)(clericCombatant)
              .useAbility(List(monster), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe (trackedCleric.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedSingleTargetSpell = trackedMeleeSpellAttack(1, higherSpellSlot = false)
          val trackedMultiTargetSpell = trackedMultiTargetSavingThrowSpell(3, Dexterity)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSingleTargetSpell, trackedMultiTargetSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castSingleTargetOffensiveSpell(Priority)(clericCombatant)
              .useAbility(List(monster), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe (trackedCleric.spellSlots.firstLevel.count - 1)
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "not spend a spell slot if cantrip was found and used" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSingleTargetSavingThrowSpell(0, Wisdom), trackedHealingSpell(3))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castSingleTargetOffensiveSpell(Priority)(clericCombatant)
              .useAbility(List(monster), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "cast cantrip if defined and no spell slots are available" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedMeleeSpellAttack(0)
          val trackedSavingThrowSpell = trackedSingleTargetSavingThrowSpell(1, Wisdom)

          val noSpellSlotsCleric = cleric
            .withSpellsKnown(trackedSpell, trackedSavingThrowSpell)
            .withNoSpellSlotsAvailable()
            .withWisdom(24)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(2).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(noSpellSlotsCleric)
            .useAbility(List(monster), LowestFirst)

          singleTargetSavingThrowSpellUsedCount shouldBe 0
          meleeSpellUsedCount shouldBe 1
        }
      }
    }

    "not meet the condition if the Spell Caster has no damaging spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withSpellKnown(CureWounds).withCombatIndex(1)

      castSingleTargetOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric]
        .withSpellsKnown(List.empty[Spell]: _*)
        .withNoSpellSlotsAvailable()
        .withCombatIndex(1)

      castSingleTargetOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster cannot cast any Single Target Damage spells at its level" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val wizardCombatant = wizard
            .withSpellsKnown(trackedMultiTargetSavingThrowSpell(1, Strength),
              trackedSingleTargetSavingThrowSpell(3, Wisdom))
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .withCombatIndex(1)

          castSingleTargetOffensiveSpell(Priority)(wizardCombatant).conditionMet shouldBe false
        }
      }
    }

    "not meet the condition if the Spell Caster only knows multi target damaging spells" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val wizardCombatant = wizard
            .withSpellsKnown(trackedMultiTargetSavingThrowSpell(1, Strength), trackedMultiTargetSavingThrowSpell(2, Strength))
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .withCombatIndex(1)

          castSingleTargetOffensiveSpell(Priority)(wizardCombatant).conditionMet shouldBe false
        }
      }
    }

    "target Player if caster is a Monster" in {
      forAll { (lich: Lich, fighter: Fighter) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val lichCombatant = lich
            .withSpellsKnown(trackedMeleeSpellAttack(2), // damage dealt is 4
              trackedMultiTargetSavingThrowSpell(3, Strength, higherSpellSlot = false)
            )
            .withCombatIndex(1)

          val easyToHitFighter = fighter
            .withHealth(100)
            .withMaxHealth(100)
            .withDexterity(2)
            .withNoArmour()
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedFighter: Fighter))) =
            castSingleTargetOffensiveSpell(Priority)(lichCombatant)
              .useAbility(List(easyToHitFighter), LowestFirst)

          meleeSpellUsedCount shouldBe 1
          meleeSpellLevelUsed shouldBe 9

          multiTargetSavingThrowSpellUsedCount shouldBe 0

          updatedFighter.health shouldBe 100 - 4
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
