package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._
import util.TestMonster

class MultiTargetSavingThrowSpellSpec extends UnitSpecBase {

  "effect" should {
    "deal full damage if saving throw failed" in {
      forAll { (cleric: Cleric, testMonsterOne: TestMonster, testMonsterTwo: TestMonster, testMonsterThree: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val savingThrowSpell = trackedMultiTargetSavingThrowSpell(1, Dexterity, damageOnSave = false)

          val trackedCleric = cleric
            .withSpellKnown(savingThrowSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val monsterOne = testMonsterOne
            .withSavingThrowScores(dexterity = -4)
            .withHealth(10)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withSavingThrowScores(dexterity = -4)
            .withHealth(10)
            .withCombatIndex(2)

          val monsterThree = testMonsterThree
            .withSavingThrowScores(dexterity = 28)
            .withHealth(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonsterOne: TestMonster), Combatant(_, updatedMonsterTwo: TestMonster), Combatant(_, updatedMonsterThree: TestMonster))) =
            savingThrowSpell.effect(trackedCleric, savingThrowSpell.spellLevel, List(monsterOne, monsterTwo, monsterThree))

          multiSavingThrowSpellUsedCount shouldBe 2
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
            .withSavingThrowScores(dexterity = -4)
            .withHealth(10)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withSavingThrowScores(dexterity = -4)
            .withHealth(10)
            .withCombatIndex(2)

          val monsterThree = testMonsterThree
            .withSavingThrowScores(dexterity = 28)
            .withHealth(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonsterOne: TestMonster), Combatant(_, updatedMonsterTwo: TestMonster), Combatant(_, updatedMonsterThree: TestMonster))) =
            savingThrowSpell.effect(trackedCleric, savingThrowSpell.spellLevel, List(monsterOne, monsterTwo, monsterThree))

          multiSavingThrowSpellUsedCount shouldBe 3
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
