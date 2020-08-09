package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastMultiTargetOffensiveSpell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.CureWounds
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.{AcidArrow, Fireball, MagicMissile}
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

class CastMultiTargetOffensiveSpellSpec extends UnitSpecBase {

  val Priority = 1

  "castMultiTargetOffensiveSpell" should {

    "cast a spell using the highest available spell slot" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedMultiTargetSavingThrowSpell(1, Wisdom)
          val trackedSingleSpell = trackedSingleTargetSavingThrowSpell(2, Strength, higherSpellSlot = false)

          val trackedWizard = wizard
            .withSpellsKnown(trackedSpell, trackedSingleSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withIntelligence(20)
            .withCombatIndex(1)

          val monsterOne = testMonsterOne
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(3)

          val (_, List(Combatant(_, updatedMonsterOne: TestMonster), Combatant(_, updatedMonsterTwo: TestMonster))) =
            castMultiTargetOffensiveSpell(Priority)(trackedWizard)
              .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          updatedMonsterOne.health < monsterOne.creature.health shouldBe true
          updatedMonsterTwo.health < monsterTwo.creature.health shouldBe true

          multiTargetSavingThrowSpellUsedCount shouldBe 1
          multiSavingThrowSpellDamageRollCount shouldBe 1
          multiTargetSavingThrowSpellLevelUsed shouldBe 3

          singleTargetSavingThrowSpellUsedCount shouldBe 0
        }
      }
    }

    "cast a spell using the lowest spell slot for a spell which does not benefit from a higher slot" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedMultiTargetSavingThrowSpell(2, Wisdom, higherSpellSlot = false)
          val trackedSingleSpell = trackedSingleTargetSavingThrowSpell(3, Strength, higherSpellSlot = false)

          val trackedWizard = wizard
            .withSpellsKnown(trackedSpell, trackedSingleSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withProficiencyBonus(6)
            .withLevel(LevelThree)
            .withIntelligence(20)
            .withCombatIndex(1)

          val monsterOne = testMonsterOne
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(3)

          val (_, List(Combatant(_, updatedMonsterOne: TestMonster), Combatant(_, updatedMonsterTwo: TestMonster))) =
            castMultiTargetOffensiveSpell(Priority)(trackedWizard)
              .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          updatedMonsterOne.health < monsterOne.creature.health shouldBe true
          updatedMonsterTwo.health < monsterTwo.creature.health shouldBe true

          multiTargetSavingThrowSpellUsedCount shouldBe 1
          multiTargetSavingThrowSpellLevelUsed shouldBe 2
          multiSavingThrowSpellDamageRollCount shouldBe 1

          singleTargetSavingThrowSpellUsedCount shouldBe 0
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedMultiTargetSpell = trackedMultiTargetSavingThrowSpell(1, Strength, higherSpellSlot = true)
          val trackedSingleTargetSpell = trackedSingleTargetSavingThrowSpell(2, Dexterity, higherSpellSlot = false)

          val trackedWizard = wizard
            .withSpellsKnown(trackedMultiTargetSpell, trackedSingleTargetSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = trackedWizard.withCombatIndex(1)

          val monsterOne = testMonsterOne
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(3)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castMultiTargetOffensiveSpell(Priority)(wizardCombatant)
              .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe trackedWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe trackedWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe (trackedWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedMultiTargetSpell = trackedMultiTargetSavingThrowSpell(1, Strength, higherSpellSlot = false)
          val trackedSingleTargetSpell = trackedMeleeSpellAttack(3)

          val trackedWizard = wizard
            .withSpellsKnown(trackedMultiTargetSpell, trackedSingleTargetSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = trackedWizard.withCombatIndex(1)

          val monsterOne = testMonsterOne
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(3)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castMultiTargetOffensiveSpell(Priority)(wizardCombatant)
              .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe (trackedWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.secondLevel.count shouldBe trackedWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe trackedWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "not meet the condition if the Spell Caster has only a damaging cantrip to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Wizard]
        .withSpellKnown(trackedSingleTargetSavingThrowSpell(0, Wisdom))
        .withSpellsKnown(List.empty[Spell]: _*)
        .withCombatIndex(1)

      castMultiTargetOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no damaging spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withSpellKnown(CureWounds).withCombatIndex(1)

      castMultiTargetOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val wizard = random[Wizard]
        .withNoSpellSlotsAvailable()
        .withCombatIndex(1)

      castMultiTargetOffensiveSpell(Priority)(wizard).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster cannot cast any Multi Attack spells at its level" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val wizardCombatant = wizard
            .withSpellsKnown(MagicMissile, AcidArrow, Fireball)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .withCombatIndex(1)

          castMultiTargetOffensiveSpell(Priority)(wizardCombatant).conditionMet shouldBe false
        }
      }
    }

    "target Players if caster is a Monster" in {
      forAll { (lich: Lich, fighter: Fighter, wizard: Wizard) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedMultiTargetSavingThrowSpell(2, Dexterity)
          val trackedSingleSpell = trackedMeleeSpellAttack(3, higherSpellSlot = false)

          val lichCombatant = lich
            .withSpellKnown(trackedSpell)
            .withCombatIndex(1)

          val easyToHitFighter = fighter
            .withHealth(100)
            .withMaxHealth(100)
            .withDexterity(2)
            .withCombatIndex(2)

          val easyToHitWizard = wizard
            .withHealth(100)
            .withMaxHealth(100)
            .withDexterity(2)
            .withCombatIndex(3)

          val (_, List(Combatant(_, updatedFighter: Fighter), Combatant(_, updatedWizard: Wizard))) =
            castMultiTargetOffensiveSpell(Priority)(lichCombatant)
              .useAbility(List(easyToHitFighter, easyToHitWizard), LowestFirst)

          updatedFighter.health < easyToHitFighter.creature.health shouldBe true
          updatedWizard.health < easyToHitWizard.creature.health shouldBe true

          multiTargetSavingThrowSpellUsedCount shouldBe 1
          multiTargetSavingThrowSpellLevelUsed shouldBe 9

          meleeSpellUsedCount shouldBe 0
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
