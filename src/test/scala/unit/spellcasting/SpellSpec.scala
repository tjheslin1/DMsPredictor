package unit.spellcasting

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import util.TestData._
import util.TestMonster

class SpellSpec extends UnitSpecBase {

  "spellOfTypeBelowLevel" should {
    "return a spell of a specific SpellEffect equal to the level given" in {
      val spellsKnown: Map[(SpellLevel, SpellEffect), Spell] = Map(
        (SacredFlame.spellLevel, SacredFlame.spellEffect) -> SacredFlame,
        (GuidingBolt.spellLevel, GuidingBolt.spellEffect) -> GuidingBolt,
        (CureWounds.spellLevel, CureWounds.spellEffect)   -> CureWounds,
        (HoldPerson.spellLevel, HoldPerson.spellEffect)   -> HoldPerson
      )

      spellOfLevelOrBelow(spellsKnown, DamageSpell, 1) shouldBe GuidingBolt.some
    }

    "return a spell of a specific SpellEffect below the level given" in {
      val spellsKnown: Map[(SpellLevel, SpellEffect), Spell] = Map(
        (SacredFlame.spellLevel, SacredFlame.spellEffect) -> SacredFlame,
        (GuidingBolt.spellLevel, GuidingBolt.spellEffect) -> GuidingBolt,
        (CureWounds.spellLevel, CureWounds.spellEffect)   -> CureWounds,
        (HoldPerson.spellLevel, HoldPerson.spellEffect)   -> HoldPerson
      )

      spellOfLevelOrBelow(spellsKnown, DamageSpell, 3) shouldBe GuidingBolt.some
    }

    "return none if no spell of SpellEffect is found" in {
      val spellsKnown: Map[(SpellLevel, SpellEffect), Spell] = Map(
        (SacredFlame.spellLevel, SacredFlame.spellEffect) -> SacredFlame,
        (GuidingBolt.spellLevel, GuidingBolt.spellEffect) -> GuidingBolt,
        (CureWounds.spellLevel, CureWounds.spellEffect)   -> CureWounds
      )

      spellOfLevelOrBelow(spellsKnown, ConditionSpell, 2) shouldBe None
    }
  }

  "spellSavingThrowPassed" should {
    "return true if the targets roll equals the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withProficiencyBonus(2).withWisdom(10).asInstanceOf[Cleric]
          val monster = testMonster.withDexterity(10)

          spellSavingThrowPassed(caster, Dexterity, monster) shouldBe true
        }
      }
    }

    "return true if the targets roll exceeds the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withProficiencyBonus(2).withWisdom(10).asInstanceOf[Cleric]
          val monster = testMonster.withDexterity(14)

          spellSavingThrowPassed(caster, Dexterity, monster) shouldBe true
        }
      }
    }

    "return false if the targets roll is less than the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withProficiencyBonus(2).withWisdom(14).asInstanceOf[Cleric]
          val monster = testMonster.withDexterity(10)

          spellSavingThrowPassed(caster, Dexterity, monster) shouldBe false
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
