package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model.SavingThrow._
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._
import util.TestMonster

class SavingThrowSpec extends UnitSpecBase {

  "savingThrowPassed" should {
    "return true if the targets roll equals the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val monster = testMonster.withDexterity(10)

          savingThrowPassed(10, Dexterity, monster) shouldBe true
        }
      }
    }

    "return true if the targets roll exceeds the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val monster = testMonster.withDexterity(14)

          savingThrowPassed(10, Dexterity, monster) shouldBe true
        }
      }
    }

    "return false if the targets roll is less than the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val monster = testMonster.withDexterity(10)

          savingThrowPassed(20, Dexterity, monster) shouldBe false
        }
      }
    }

    "use Player's saving throw proficiencies" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val proficientFighter = fighter.withStrength(10).asInstanceOf[Player]

          val dc = 10 + proficientFighter.proficiencyBonus
          savingThrowPassed(dc, Strength, proficientFighter) shouldBe true
        }
      }
    }

    "use Monster's saving throw proficiencies" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val monster = testMonster.withSavingThrowScores(strength = 5)

          val dc = 12
          savingThrowPassed(dc, Strength, monster) shouldBe true
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
