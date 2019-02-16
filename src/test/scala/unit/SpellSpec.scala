package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellSavingThrowPassed
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.SacredFlame
import util.TestData._
import util.TestMonster

class SpellSpec extends UnitSpecBase {

  "spellSavingThrowPassed" should {
    "return true if the targets roll equals the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withWisdom(10).withProficiencyBonus(2)
          val monster = testMonster.withDexterity(10)

          spellSavingThrowPassed(caster, SacredFlame, Dexterity, monster) shouldBe true
        }
      }
    }

    "return true if the targets roll exceeds the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withWisdom(10).withProficiencyBonus(2)
          val monster = testMonster.withDexterity(14)

          spellSavingThrowPassed(caster, SacredFlame, Dexterity, monster) shouldBe true
        }
      }
    }

    "return false if the targets roll is less than the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withWisdom(14).withProficiencyBonus(2)
          val monster = testMonster.withDexterity(10)

          spellSavingThrowPassed(caster, SacredFlame, Dexterity, monster) shouldBe false
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
