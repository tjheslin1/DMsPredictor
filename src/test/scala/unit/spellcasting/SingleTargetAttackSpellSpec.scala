package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Paralyzed
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._
import util.TestMonster

class SingleTargetAttackSpellSpec extends UnitSpecBase {

  "effect" should {
    "apply damage to target if Hit" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val fireAttackSpell = trackedMeleeSpellAttack(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withDamageResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = fireAttackSpell.effect(
            fireSpellCleric,
            fireAttackSpell.spellLevel,
            List(monster))

          meleeSpellUsedCount shouldBe 1
        }
      }
    }

    "apply damage twice to target if CriticalHit" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val fireAttackSpell = trackedMeleeSpellAttack(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withDamageResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = fireAttackSpell.effect(
            fireSpellCleric,
            fireAttackSpell.spellLevel,
            List(monster))

          meleeSpellUsedCount shouldBe 2
        }
      }
    }

    "not apply damage to target if Miss and halfDamageOnMiss is set to false" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(2)

          val fireAttackSpell = trackedMeleeSpellAttack(1, halfDamageOnAMiss = false)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .withWisdom(10)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = fireAttackSpell.effect(
            fireSpellCleric,
            fireAttackSpell.spellLevel,
            List(monster))

          meleeSpellUsedCount shouldBe 0
        }
      }
    }

    "deal half damage on miss" in {
      forAll { (wizard: Wizard, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val halfDamageOnSaveSpell = trackedMeleeSpellAttack(1, dmg = 10, halfDamageOnAMiss = true)

          val missingWizard = wizard
            .withSpellKnown(halfDamageOnSaveSpell)
            .withIntelligence(1)
            .asInstanceOf[Wizard]

          val monster = testMonster
            .withArmourClass(30)
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = halfDamageOnSaveSpell.effect(
            missingWizard,
            halfDamageOnSaveSpell.spellLevel,
            List(monster))

          updatedMonster.health shouldBe monster.creature.health - (10 / 2)
        }
      }
    }

    "not apply damage to target if CriticalMiss" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(1)

          val fireAttackSpell = trackedMeleeSpellAttack(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withDamageResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = fireAttackSpell.effect(
            fireSpellCleric,
            fireAttackSpell.spellLevel,
            List(monster))

          meleeSpellUsedCount shouldBe 0
        }
      }
    }

    "deal half damage to a creature resistant to the damage type" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val fireAttackSpell = trackedMeleeSpellAttack(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withDamageResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = fireAttackSpell.effect(
            fireSpellCleric,
            fireAttackSpell.spellLevel,
            List(monster))

          updatedMonster.health shouldBe 8
        }
      }
    }

    "deal zero damage to a creature immune to the damage type" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val fireAttackSpell = trackedMeleeSpellAttack(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withDamageImmunity(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = fireAttackSpell.effect(
            fireSpellCleric,
            fireAttackSpell.spellLevel,
            List(monster))

          updatedMonster.health shouldBe 10
        }
      }
    }

    "apply additional effect" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val applyParalysis: Combatant => Combatant =
            c =>
              (Combatant.creatureLens composeLens Creature.creatureConditionsLens).set(
                List(Paralyzed(10, 10, Strength)))(c)

          val spellAttackWithEffectSpell = trackedMeleeSpellAttack(
            1,
            additionalTargetEffect = applyParalysis)

          val trackedCleric = cleric
            .withSpellKnown(spellAttackWithEffectSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val monster = goblin
            .withHealth(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) = spellAttackWithEffectSpell.effect(
            trackedCleric,
            spellAttackWithEffectSpell.spellLevel,
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
