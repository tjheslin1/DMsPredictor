package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Paralyzed
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._
import util.TestMonster

class SingleTargetSavingThrowSpellSpec extends UnitSpecBase {

  "effect" should {
    "deal full damage if saving throw failed" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val savingThrowSpell = trackedSingleTargetSavingThrowSpell(
            1,
            Dexterity,
            damageOnSave = false)

          val trackedCleric = cleric
            .withSpellKnown(savingThrowSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withSavingThrowScores(dexterity = -4)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = savingThrowSpell.effect(
            trackedCleric,
            savingThrowSpell.spellLevel,
            List(monster))

          singleTargetSavingThrowSpellUsedCount shouldBe 1
          updatedMonster.health shouldBe monster.creature.health - 4
        }
      }
    }

    "deal half damage (rounded down) if saving throw passed and half damage on save is true" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val savingThrowSpell = trackedSingleTargetSavingThrowSpell(
            1,
            Dexterity,
            damageOnSave = true)

          val trackedCleric = cleric
            .withSpellKnown(savingThrowSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(2)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withDexterity(15)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = savingThrowSpell.effect(
            trackedCleric,
            savingThrowSpell.spellLevel,
            List(monster))

          singleTargetSavingThrowSpellUsedCount shouldBe 1
          updatedMonster.health shouldBe monster.creature.health - 2
        }
      }
    }

    "deal no damage if saving throw passed and half damage on save is false" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val savingThrowSpell = trackedSingleTargetSavingThrowSpell(
            1,
            Dexterity,
            damageOnSave = false)

          val trackedCleric = cleric
            .withSpellKnown(savingThrowSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(2)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withDexterity(15)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = savingThrowSpell.effect(
            trackedCleric,
            savingThrowSpell.spellLevel,
            List(monster))

          singleTargetSavingThrowSpellUsedCount shouldBe 0
          updatedMonster.health shouldBe monster.creature.health
        }
      }
    }

    "apply additional effect" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val applyParalysis: (SpellCaster, Combatant, List[Combatant], Boolean) => (
              SpellCaster,
              Combatant,
              List[Combatant]) =
            (spellCaster, target, others, _) => {
              val updatedTarget =
                (Combatant.creatureLens composeLens Creature.creatureConditionsLens).set(
                  List(Paralyzed(10, 10, Strength)))(target)

              (spellCaster, updatedTarget, others)
            }

          val savingThrowSpell = trackedSingleTargetSavingThrowSpell(
            1,
            Dexterity,
            additionalTargetEffect = applyParalysis)

          val trackedCleric = cleric
            .withSpellKnown(savingThrowSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val monster = goblin
            .withStrength(1)
            .withHealth(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) = savingThrowSpell.effect(
            trackedCleric,
            savingThrowSpell.spellLevel,
            List(monster))

          updatedGoblin.conditions should contain theSameElementsAs List(
            Paralyzed(10, 10, Strength))
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
