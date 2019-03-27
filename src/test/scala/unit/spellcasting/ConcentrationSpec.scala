package unit.spellcasting

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Turned}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Concentration._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import util.TestData._

class ConcentrationSpec extends UnitSpecBase {

  "concentrationDifficultyClass" should {
    "set the DC to 10 if half the damage taken is less than 10" in {
      concentrationDifficultyClass(5) shouldBe 10
    }

    "set the DC to half the damage taken if more than 10" in {
      concentrationDifficultyClass(22) shouldBe 11
    }
  }

  "handleConcentration" should {
    "break concentration if check failed" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(8)

          val lowConstitutionCleric = cleric
            .withConcentrating(concentrationSpell.some)
            .withConstitution(5)
            .asInstanceOf[SpellCaster]

          val updatedCleric = handleConcentration(lowConstitutionCleric, damageTaken = 20)

          updatedCleric.isConcentrating shouldBe false
        }
      }
    }

    "maintain concentration if check passed" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(8)

          val highConstitutionCleric = cleric
            .withConcentrating(concentrationSpell.some)
            .withConstitution(18)
            .asInstanceOf[SpellCaster]

          val updatedCleric = handleConcentration(highConstitutionCleric, damageTaken = 10)

          updatedCleric.isConcentrating shouldBe true
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

    val concentrationSpell: Spell = new ConcentrationConditionSpell() {
      val name: String                   = "test-concentration-spell"

      val attribute: Attribute           = Wisdom
      val singleTarget: Boolean = true

      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneAction
      val spellLevel: SpellLevel         = 1

      def conditionFrom(spellCaster: SpellCaster): Condition = Turned(10, 10)
    }
  }
}
