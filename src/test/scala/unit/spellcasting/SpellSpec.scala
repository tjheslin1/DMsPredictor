package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import util.TestData._
import util.TestMonster

class SpellSpec extends UnitSpecBase {

  "spellOfTypeBelowLevel" should {

    "return a spell of a specific SpellEffect equal to the level given" in {
      val cleric = random[Cleric].withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(cleric, DamageSpell, 1) shouldBe GuidingBolt.some
    }

    "return a multi attack spell of a specific SpellEffect equal to the level given" in {
      val wizard = random[Wizard].withSpellsKnown(FireBolt, MagicMissile, AcidArrow, Fireball)

      spellOfLevelOrBelow(wizard, DamageSpell, 3, multiAttackOnly = true) shouldBe Fireball.some
    }

    "return a spell of a specific SpellEffect below the level given" in {
      val cleric = random[Cleric].withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(cleric, DamageSpell, 3) shouldBe GuidingBolt.some
    }

    "return a multi attack spell of a specific SpellEffect below the level given" in new Tracking {
      val trackedLevelTwoMultiSpell = trackedMultiMeleeSpellAttack(2)

      val cleric = random[Cleric].withSpellsKnown(FireBolt, MagicMissile, trackedLevelTwoMultiSpell, trackedSingleTargetSavingThrowSpell(3, Wisdom))

      spellOfLevelOrBelow(cleric, DamageSpell, 3, multiAttackOnly = true) shouldBe trackedLevelTwoMultiSpell.some
    }

    "return a concentration spell if already concentrating when checkConcentration is set to false" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val concentratingCleric = random[Cleric]
        .withConcentrating(trackedConditionSpell(1).some)
        .withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(concentratingCleric, ConcentrationSpell, 3, checkConcentration = false) shouldBe None
    }

    "not return a concentration spell if already concentrating when checkConcentration is set to true" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val concentratingCleric = random[Cleric]
        .withConcentrating(trackedConditionSpell(1).some)
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

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
