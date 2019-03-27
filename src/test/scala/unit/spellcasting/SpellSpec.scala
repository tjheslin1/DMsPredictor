package unit.spellcasting

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Turned}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import util.TestData._
import util.TestMonster

class SpellSpec extends UnitSpecBase {

  "spellOfTypeBelowLevel" should {
    "return a spell of a specific SpellEffect equal to the level given" in {
      val cleric = random[Cleric].withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(cleric, DamageSpell, 1) shouldBe GuidingBolt.some
    }

    "return a spell of a specific SpellEffect below the level given" in {
      val cleric = random[Cleric].withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(cleric, DamageSpell, 3) shouldBe GuidingBolt.some
    }

    "return a concentration spell if already concentrating when checkConcentration is set to false" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val concentratingCleric = random[Cleric]
        .withConcentrating(concentrationSpell.some)
        .withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(concentratingCleric, ConcentrationSpell, 3, checkConcentration = false) shouldBe None
    }

    "not return a concentration spell if already concentrating when checkConcentration is set to true" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val concentratingCleric = random[Cleric]
        .withConcentrating(concentrationSpell.some)
        .withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(concentratingCleric, ConcentrationSpell, 3) shouldBe None
    }

    "return none if no spell of SpellEffect is found" in {
      val cleric = random[Cleric].withSpellsKnown(SacredFlame, GuidingBolt, CureWounds)

      spellOfLevelOrBelow(cleric, ConcentrationSpell, 2) shouldBe None
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

  abstract private class TestContext {
    implicit val roll: RollStrategy

    val concentrationSpell: Spell = new ConcentrationConditionSpell() {
      val name: String = "test-concentration-spell"

      val attribute: Attribute  = Wisdom
      val singleTarget: Boolean = true

      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneAction
      val spellLevel: SpellLevel         = 1

      def conditionFrom(spellCaster: SpellCaster): Condition = Turned(10, 10)
    }
  }
}
