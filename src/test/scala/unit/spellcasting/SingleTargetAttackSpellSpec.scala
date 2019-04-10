package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._
import util.TestMonster

class SingleTargetAttackSpellSpec extends UnitSpecBase {

  "effect" should {
    "apply damage to target if Hit" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val fireAttackSpell = trackedMeleeSpellAttack(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, fireAttackSpell.spellLevel, List(monster))

          meleeSpellUsedCount shouldBe 1
        }
      }
    }

    "apply damage twice to target if CriticalHit" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = D20.naturalTwenty

          val fireAttackSpell = trackedMeleeSpellAttack(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, fireAttackSpell.spellLevel, List(monster))

          meleeSpellUsedCount shouldBe 2
        }
      }}

    "not apply damage to target if Miss" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(2)

          val fireAttackSpell = trackedMeleeSpellAttack(1)

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

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, fireAttackSpell.spellLevel, List(monster))

          meleeSpellUsedCount shouldBe 0
        }
      }}

    "not apply damage to target if CriticalMiss" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(1)

          val fireAttackSpell = trackedMeleeSpellAttack(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, fireAttackSpell.spellLevel, List(monster))

          meleeSpellUsedCount shouldBe 0
        }
      }}

    "deal half damage to a creature resistant to the damage type" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val fireAttackSpell = trackedMeleeSpellAttack(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, fireAttackSpell.spellLevel, List(monster))

          updatedMonster.health shouldBe 8
        }
      }
    }

    "deal zero damage to a creature immune to the damage type" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val fireAttackSpell = trackedMeleeSpellAttack(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withImmunity(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, fireAttackSpell.spellLevel, List(monster))

          updatedMonster.health shouldBe 10
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
