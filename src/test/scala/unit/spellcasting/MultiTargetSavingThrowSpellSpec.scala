package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._
import util.TestMonster

class MultiTargetSavingThrowSpellSpec extends UnitSpecBase {

  "effect" should {
    "roll the damage once for all targets" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val savingThrowSpell = trackedMultiTargetSavingThrowSpell(1, Dexterity, damageOnSave = false)

      val cleric = random[Cleric]
        .withSpellKnown(savingThrowSpell)
        .withAllSpellSlotsAvailableForLevel(LevelOne)
        .withLevel(LevelOne)
        .withWisdom(15)
        .asInstanceOf[Cleric]

      val monsterOne = random[TestMonster].withHealth(10).withSavingThrowScores(dexterity = -4).withCombatIndex(2)
      val monsterTwo = random[TestMonster].withHealth(10).withSavingThrowScores(dexterity = -4).withCombatIndex(3)

      savingThrowSpell.effect(cleric, savingThrowSpell.spellLevel, List(monsterOne, monsterTwo))

      multiSavingThrowSpellUsedCount shouldBe 1
    }

    "deal full damage if saving throw failed" in {
      forAll { (cleric: Cleric, testMonsterOne: TestMonster, testMonsterTwo: TestMonster, testMonsterThree: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val savingThrowSpell = trackedMultiTargetSavingThrowSpell(1, Dexterity, damageOnSave = false)

          val trackedCleric = cleric
            .withSpellKnown(savingThrowSpell)
            .withAllSpellSlotsAvailableForLevel(LevelOne)
            .withChannelDivinityUsed()
            .withLevel(LevelOne)
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val monsterOne = testMonsterOne.withHealth(10).withSavingThrowScores(dexterity = -4).withCombatIndex(2)

          val monsterTwo = testMonsterTwo.withHealth(10).withSavingThrowScores(dexterity = -4).withCombatIndex(2)

          val monsterThree = testMonsterThree.withHealth(10).withSavingThrowScores(dexterity = 28).withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonsterOne: TestMonster), Combatant(_, updatedMonsterTwo: TestMonster), Combatant(_, updatedMonsterThree: TestMonster))) =
            savingThrowSpell.effect(trackedCleric, savingThrowSpell.spellLevel, List(monsterOne, monsterTwo, monsterThree))

          updatedMonsterOne.health shouldBe monsterOne.creature.health - 4
          updatedMonsterTwo.health shouldBe monsterTwo.creature.health - 4
          updatedMonsterThree.health shouldBe monsterThree.creature.health
        }
      }
    }

    "deal half damage (rounded down) if saving throw passed and half damage on save is true" in {
      forAll { (cleric: Cleric, testMonsterOne: TestMonster, testMonsterTwo: TestMonster, testMonsterThree: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val savingThrowSpell = trackedMultiTargetSavingThrowSpell(1, Dexterity, damageOnSave = true)

          val trackedCleric = cleric
            .withSpellKnown(savingThrowSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(8)
            .asInstanceOf[Cleric]

          val monsterOne = testMonsterOne
            .withHealth(10)
            .withSavingThrowScores(dexterity = -4)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withHealth(10)
            .withSavingThrowScores(dexterity = -4)
            .withCombatIndex(2)

          val monsterThree = testMonsterThree
            .withHealth(10)
            .withSavingThrowScores(dexterity = 28)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonsterOne: TestMonster), Combatant(_, updatedMonsterTwo: TestMonster), Combatant(_, updatedMonsterThree: TestMonster))) =
            savingThrowSpell.effect(trackedCleric, savingThrowSpell.spellLevel, List(monsterOne, monsterTwo, monsterThree))

          updatedMonsterOne.health shouldBe monsterOne.creature.health - 4
          updatedMonsterTwo.health shouldBe monsterTwo.creature.health - 4
          updatedMonsterThree.health shouldBe monsterThree.creature.health - 2
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
