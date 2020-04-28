package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model.SavingThrow._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Stunned
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._
import util.TestMonster

class SavingThrowSpec extends UnitSpecBase {

  "savingThrowPassed" should {
    "return true if the target's roll equals the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val monster = testMonster.withDexterity(10)

          savingThrowPassed(10, Dexterity, monster) shouldBe true
        }
      }
    }

    "return true if the target's roll exceeds the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val monster = testMonster.withDexterity(14)

          savingThrowPassed(10, Dexterity, monster) shouldBe true
        }
      }
    }

    "return false if the target's roll is less than the caster's spell save DC" in {
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

    "automatically fail a Strength saving throw for a Stunned creature" in {
      forAll { goblin: Goblin =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val stunnedGoblin = goblin.withStrength(10).withCondition(Stunned(20))

          val dc = 5
          savingThrowPassed(dc, Strength, stunnedGoblin) shouldBe false
        }
      }
    }

    "automatically fail a Dexterity saving throw for a Stunned creature" in {
      forAll { goblin: Goblin =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val stunnedGoblin = goblin.withDexterity(10).withCondition(Stunned(20))

          val dc = 5
          savingThrowPassed(dc, Dexterity, stunnedGoblin) shouldBe false
        }
      }
    }

    "make regular saving throws whilst Stunned for Constitution, Intelligence, Wisdom, Charisma" in {
      forAll { goblin: Goblin =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val stunnedGoblin = goblin.withStats(BaseStats(10, 10, 10, 10, 10, 10)).withCondition(Stunned(20))

          val dc = 5
          savingThrowPassed(dc, Constitution, stunnedGoblin) shouldBe true
          savingThrowPassed(dc, Intelligence, stunnedGoblin) shouldBe true
          savingThrowPassed(dc, Wisdom, stunnedGoblin) shouldBe true
          savingThrowPassed(dc, Charisma, stunnedGoblin) shouldBe true
        }
      }
    }
  }

  "savingThrowWithAdvantagePassed" should {
    "return true if the target's roll equals the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          val iterator = Iterator(1, 10)
          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val monster = testMonster.withDexterity(10)

          savingThrowWithAdvantagePassed(10, Dexterity, monster) shouldBe true
        }
      }
    }

    "return true if the target's roll exceeds the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          val iterator = Iterator(1, 10)
          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val monster = testMonster.withDexterity(14)

          savingThrowWithAdvantagePassed(10, Dexterity, monster) shouldBe true
        }
      }
    }

    "return false if the target's roll is less than the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          val iterator = Iterator(1, 2)
          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val monster = testMonster.withDexterity(10)

          savingThrowWithAdvantagePassed(20, Dexterity, monster) shouldBe false
        }
      }
    }
  }

  "savingThrowWithDisadvantagePassed" should {
    "return true if the target's roll equals the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          val iterator = Iterator(15, 10)
          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val monster = testMonster.withDexterity(10)

          savingThrowWithDisadvantagePassed(10, Dexterity, monster) shouldBe true
        }
      }
    }

    "return true if the target's roll exceeds the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          val iterator = Iterator(15, 10)
          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val monster = testMonster.withDexterity(14)

          savingThrowWithDisadvantagePassed(10, Dexterity, monster) shouldBe true
        }
      }
    }

    "return false if the target's roll is less than the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          val iterator = Iterator(20, 1)
          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val monster = testMonster.withDexterity(10)

          savingThrowWithDisadvantagePassed(20, Dexterity, monster) shouldBe false
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
