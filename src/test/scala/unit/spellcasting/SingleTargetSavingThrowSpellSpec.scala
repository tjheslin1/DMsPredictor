package unit.spellcasting

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import util.TestMonster
import util.TestData._

class SingleTargetSavingThrowSpellSpec extends UnitSpecBase {

  "effect" should {
    "deal full damage if saving throw failed" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val savingThrowSpell = dexteritySavingThrowSpell(damageOnSave = false)

          val fireSpellCleric = cleric
            .withSpellKnown(savingThrowSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withDexterity(2)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            savingThrowSpell.effect(fireSpellCleric, List(monster))

          dexteritySaveDamageCount shouldBe 1
          updatedMonster.health shouldBe monster.creature.health - 4
        }
      }
    }

    "deal half damage (rounded down) if saving throw passed and half damage on save is true" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val savingThrowSpell = dexteritySavingThrowSpell(damageOnSave = true)

          val fireSpellCleric = cleric
            .withSpellKnown(savingThrowSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(2)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withDexterity(15)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            savingThrowSpell.effect(fireSpellCleric, List(monster))

          dexteritySaveDamageCount shouldBe 1
          updatedMonster.health shouldBe monster.creature.health - 2
        }
      }
    }

    "deal no damage if saving throw passed and half damage on save is false" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val savingThrowSpell = dexteritySavingThrowSpell(damageOnSave = false)

          val fireSpellCleric = cleric
            .withSpellKnown(savingThrowSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(2)
            .asInstanceOf[Cleric]

          val monster = testMonster
            .withHealth(10)
            .withDexterity(15)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            savingThrowSpell.effect(fireSpellCleric, List(monster))

          dexteritySaveDamageCount shouldBe 0
          updatedMonster.health shouldBe monster.creature.health
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

    var dexteritySaveDamageCount = 0
    def dexteritySavingThrowSpell(damageOnSave: Boolean) = new SingleTargetSavingThrowSpell() {
      val attribute: Attribute = Dexterity
      val halfDamageOnSave: Boolean = damageOnSave

      val damageType: DamageType   = Fire
      val name: String             = "tracked-dexterity-save-spell"
      val school: SchoolOfMagic    = Evocation
      val castingTime: CastingTime = OneAction
      val spellLevel: SpellLevel   = 1
      val concentration: Boolean   = false

      def damage[_: RS](spellCaster: SpellCaster): Int = {
        dexteritySaveDamageCount += 1
        4
      }
    }
  }
}
