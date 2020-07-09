package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class ConditionSpellSpec extends UnitSpecBase {

  "effect" should {
    "apply condition to first enemy if single target spell and saving throw failed" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin, goblinThree: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterityConditionSpell = trackedConditionSpell(1, numTargets = 1, savingThrowAttribute = Dexterity)

          val conditionSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val (_,
               List(Combatant(_, updatedGoblinOne: Goblin),
                    Combatant(_, updatedGoblinTwo: Goblin),
                    Combatant(_, updatedGoblinThree: Goblin))) =
            dexterityConditionSpell.effect(conditionSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(lowDexGoblin(goblinOne, 2),
                                                lowDexGoblin(goblinTwo, 3),
                                                lowDexGoblin(goblinThree, 4)))

          val expectedCondition = List(dexterityConditionSpell.conditionFrom(conditionSpellCleric))

          conditionSpellUsedCount shouldBe 1

          updatedGoblinOne.conditions shouldBe goblinOne.creature.conditions ++ expectedCondition
          updatedGoblinTwo.conditions shouldBe goblinTwo.creature.conditions
          updatedGoblinThree.conditions shouldBe goblinThree.creature.conditions
        }
      }
    }

    "apply condition to all enemies if saving throw failed" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin, goblinThree: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterityConditionSpell = trackedConditionSpell(1, numTargets = 3, savingThrowAttribute = Dexterity)

          val conditionSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val (_,
               List(Combatant(_, updatedGoblinOne: Goblin),
                    Combatant(_, updatedGoblinTwo: Goblin),
                    Combatant(_, updatedGoblinThree: Goblin))) =
            dexterityConditionSpell.effect(conditionSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(lowDexGoblin(goblinOne, 2),
                                                lowDexGoblin(goblinTwo, 3),
                                                lowDexGoblin(goblinThree, 4)))

          val expectedCondition = List(dexterityConditionSpell.conditionFrom(conditionSpellCleric))

          conditionSpellUsedCount shouldBe 3

          updatedGoblinOne.conditions shouldBe goblinOne.creature.conditions ++ expectedCondition
          updatedGoblinTwo.conditions shouldBe goblinTwo.creature.conditions ++ expectedCondition
          updatedGoblinThree.conditions shouldBe goblinThree.creature.conditions ++ expectedCondition
        }
      }
    }

    "not apply condition to enemies if saving throw passed" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin, goblinThree: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterityConditionSpell = trackedConditionSpell(1, numTargets = 3, savingThrowAttribute = Dexterity)

          val conditionSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val (_,
               List(Combatant(_, updatedGoblinOne: Goblin),
                    Combatant(_, updatedGoblinTwo: Goblin),
                    Combatant(_, updatedGoblinThree: Goblin))) =
            dexterityConditionSpell.effect(conditionSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(lowDexGoblin(goblinOne, 2),
                                                lowDexGoblin(goblinTwo, 3),
                                                highDexGoblin(goblinThree, 4)))

          val expectedCondition = List(dexterityConditionSpell.conditionFrom(conditionSpellCleric))

          conditionSpellUsedCount shouldBe 2

          updatedGoblinOne.conditions shouldBe goblinOne.creature.conditions ++ expectedCondition
          updatedGoblinTwo.conditions shouldBe goblinTwo.creature.conditions ++ expectedCondition
          updatedGoblinThree.conditions shouldBe goblinThree.creature.conditions
        }
      }
    }

    "set cleric to concentrating on spell if at least one enemy failed the saving throw" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterityConditionSpell = trackedConditionSpell(1, numTargets = 3, savingThrowAttribute = Dexterity)

          val conditionSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val slowGoblin  = lowDexGoblin(goblinOne, 2)
          val quickGoblin = highDexGoblin(goblinTwo, 3)

          val (updatedCleric: Cleric, _) =
            dexterityConditionSpell.effect(conditionSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(slowGoblin, quickGoblin))

          updatedCleric.isConcentrating shouldBe true
          updatedCleric.concentratingSpell shouldBe dexterityConditionSpell.some
        }
      }
    }

    "not set cleric to concentrating on spell if all enemies passed the saving throw" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterityConditionSpell = trackedConditionSpell(1, numTargets = 3, savingThrowAttribute = Dexterity)

          val conditionSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(5)
            .asInstanceOf[Cleric]

          val slowGoblin  = highDexGoblin(goblinOne, 2)
          val quickGoblin = highDexGoblin(goblinTwo, 3)

          val (updatedCleric: Cleric, _) =
            dexterityConditionSpell.effect(conditionSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(slowGoblin, quickGoblin))

          updatedCleric.isConcentrating shouldBe false
          updatedCleric.concentratingSpell shouldBe none[Spell]
        }
      }
    }
  }

  private def lowDexGoblin(goblin: Goblin, combatIndex: Int) =
    goblin.withDexterity(2).withCombatIndex(combatIndex)

  private def highDexGoblin(goblin: Goblin, combatIndex: Int) =
    goblin.withDexterity(26).withCombatIndex(combatIndex)

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
