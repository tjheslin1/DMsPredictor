package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastConditionSpell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.MagicMissile
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class CastConditionSpellSpec extends UnitSpecBase {

  val Priority = 1

  "castConditionSpell" should {
    "cast a spell (condition)" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedSpell = trackedConditionSpell(2)

          val trackedCleric = cleric
            .withSpellKnown(trackedSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withCombatIndex(1)

          val goblinCombatant = goblin.withCombatIndex(2)

          castConditionSpell(Priority)(trackedCleric)
            .useAbility(List(goblinCombatant), LowestFirst)

          conditionSpellUsedCount shouldBe 1
          conditionSpellLevelUsed shouldBe 2
        }
      }
    }

    "set the spellCasters concentration to the cast spell for a concentration spell" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(1)

          val trackedSpell = trackedConditionSpell(
            2,
            savingThrowAttribute = Wisdom,
            concentration = true)

          val trackedCleric = cleric
            .withSpellKnown(trackedSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withWisdom(20)
            .withCombatIndex(1)

          val monster = goblin.withWisdom(2).withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) = castConditionSpell(Priority)(trackedCleric)
            .useAbility(List(monster), LowestFirst)

          conditionSpellUsedCount shouldBe 1
          conditionSpellLevelUsed shouldBe 3

          updatedCleric.concentratingSpell shouldBe trackedSpell.some
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedMultiConditionSpell = trackedConditionSpell(
            1,
            concentration = false,
            higherSpellSlot = true)

          val trackedCleric = cleric
            .withSpellsKnown(trackedMultiConditionSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val goblinCombatantOne = goblinOne.withCombatIndex(2)
          val goblinCombatantTwo = goblinTwo.withCombatIndex(3)

          val (Combatant(_, updatedCleric: Cleric), _) = castConditionSpell(Priority)(
            clericCombatant)
            .useAbility(List(goblinCombatantOne, goblinCombatantTwo), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count - 1
        }
      }
    }

    "spend the highest available spell slot for a concentration spell" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedConcentrationSpell = trackedConditionSpell(
            1,
            concentration = true,
            higherSpellSlot = true)

          val trackedCleric = cleric
            .withSpellsKnown(trackedConcentrationSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val goblinCombatantOne = goblinOne.withCombatIndex(2)
          val goblinCombatantTwo = goblinTwo.withCombatIndex(3)

          val (Combatant(_, updatedCleric: Cleric), _) = castConditionSpell(Priority)(
            clericCombatant)
            .useAbility(List(goblinCombatantOne, goblinCombatantTwo), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count - 1
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedSpell = trackedConditionSpell(
            2,
            concentration = false,
            higherSpellSlot = false)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val goblinCombatantOne = goblinOne.withCombatIndex(2)
          val goblinCombatantTwo = goblinTwo.withCombatIndex(3)

          val (Combatant(_, updatedCleric: Cleric), _) = castConditionSpell(Priority)(
            clericCombatant)
            .useAbility(List(goblinCombatantOne, goblinCombatantTwo), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count - 1
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "spend the lowest available spell slot necessary for a concentration spell which does not benefit from a higher slot" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedConcentrationSpell = trackedConditionSpell(
            2,
            concentration = true,
            higherSpellSlot = false)

          val trackedCleric = cleric
            .withSpellsKnown(trackedConcentrationSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val goblinCombatantOne = goblinOne.withCombatIndex(2)
          val goblinCombatantTwo = goblinTwo.withCombatIndex(3)

          val (Combatant(_, updatedCleric: Cleric), _) = castConditionSpell(Priority)(
            clericCombatant)
            .useAbility(List(goblinCombatantOne, goblinCombatantTwo), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count - 1
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "not meet the condition if the Spell Caster has no condition spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withSpellKnown(MagicMissile).withCombatIndex(1)

      castConditionSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric]
        .withNoSpellSlotsAvailable()
        .withCombatIndex(1)

      castConditionSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster is concentrating and only has another concentration condition spell to cast" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell  = trackedConditionSpell(2, concentration = true)
          val trackedSpell2 = trackedConditionSpell(3, concentration = true)

          val concentratingCleric = cleric
            .withSpellsKnown(trackedSpell, trackedSpell2)
            .withConcentratingOn(trackedSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withCombatIndex(1)

          castConditionSpell(Priority)(concentratingCleric).conditionMet shouldBe false
        }
      }
    }

    "target Player if caster is a Monster" in {
      forAll { (lich: Lich, fighter: Fighter) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val lichCombatant = lich
            .withSpellKnown(trackedConditionSpell(2, savingThrowAttribute = Dexterity))
            .withCombatIndex(1)

          val easyToHitFighter = fighter.withDexterity(2).withCombatIndex(2)

          castConditionSpell(Priority)(lichCombatant)
            .useAbility(List(easyToHitFighter), LowestFirst)

          conditionSpellUsedCount shouldBe 1
          conditionSpellLevelUsed shouldBe 9
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
