package unit.spellcasting

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import util.TestData._
import util.TestMonster

class SingleTargetAttackSpellSpec extends UnitSpecBase {

  "effect" should {
    "apply damage to target if Hit" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailable()
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, List(monster))

          fireAttackDamageCount shouldBe 1
        }
      }
    }

    "apply damage twice to target if CriticalHit" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailable()
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, List(monster))

          fireAttackDamageCount shouldBe 2
        }
      }}

    "not apply damage to target if Miss" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(2)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailable()
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .withWisdom(10)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, List(monster))

          fireAttackDamageCount shouldBe 0
        }
      }}

    "not apply damage to target if CriticalMiss" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(1)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailable()
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, List(monster))

          fireAttackDamageCount shouldBe 0
        }
      }}

    "deal half damage to a creature resistant to the damage type" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailable()
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, List(monster))

          updatedMonster.health shouldBe 8
        }
      }
    }

    "deal zero damage to a creature immune to the damage type" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val fireSpellCleric = cleric
            .withSpellKnown(fireAttackSpell)
            .withAllSpellSlotsAvailable()
            .withChannelDivinityUsed()
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withImmunity(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            fireAttackSpell.effect(fireSpellCleric, List(monster))

          updatedMonster.health shouldBe 10
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

    var fireAttackDamageCount = 0
    val fireAttackSpell = new SingleTargetAttackSpell() {
      val damageType: DamageType   = Fire
      val name: String             = "tracked-fire-spell"
      val school: SchoolOfMagic    = Evocation
      val castingTime: CastingTime = OneAction
      val spellLevel: SpellLevel   = 1
      val concentration: Boolean   = false

      def damage[_: RS](spellCaster: SpellCaster): Int = {
        fireAttackDamageCount += 1
        4
      }
    }
  }
}
