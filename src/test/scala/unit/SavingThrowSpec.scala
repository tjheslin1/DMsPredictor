package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model.SavingThrow._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Stunned
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import util.TestData._
import util.TestMonster

class SavingThrowSpec extends UnitSpecBase {

  "savingThrowPassed" should {
    "return true if the target's roll equals the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val monster = testMonster.withDexterity(10)

          val (result, _) = savingThrowPassed(10, Dexterity, monster)

          result shouldBe true
        }
      }
    }

    "return true if the target's roll exceeds the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val monster = testMonster.withDexterity(14)

          val (result, _) = savingThrowPassed(10, Dexterity, monster)

          result shouldBe true
        }
      }
    }

    "return false if the target's roll is less than the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val monster = testMonster.withDexterity(10)

          val (result, _) = savingThrowPassed(20, Dexterity, monster)

          result shouldBe false
        }
      }
    }

    "use Player's saving throw proficiencies" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val proficientFighter = fighter.withStrength(10).asInstanceOf[Player]

          val dc = 10 + proficientFighter.proficiencyBonus

          val (result, _) = savingThrowPassed(dc, Strength, proficientFighter)

          result shouldBe true
        }
      }
    }

    "use Monster's saving throw proficiencies" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val monster = testMonster.withSavingThrowScores(strength = 5)

          val dc = 12
          val (result, _) = savingThrowPassed(dc, Strength, monster)

          result shouldBe true
        }
      }
    }

    "automatically fail a Strength saving throw for a Stunned creature" in {
      forAll { goblin: Goblin =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val stunnedGoblin = goblin.withStrength(10).withCondition(Stunned(20))

          val dc = 5
          val (result, _) = savingThrowPassed(dc, Strength, stunnedGoblin)

          result shouldBe false
        }
      }
    }

    "automatically fail a Dexterity saving throw for a Stunned creature" in {
      forAll { goblin: Goblin =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val stunnedGoblin = goblin.withDexterity(10).withCondition(Stunned(20))

          val dc = 5
          val (result, _) = savingThrowPassed(dc, Dexterity, stunnedGoblin)

          result shouldBe false
        }
      }
    }

    "make regular saving throws whilst Stunned for Constitution, Intelligence, Wisdom, Charisma" in {
      forAll { goblin: Goblin =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val stunnedGoblin = goblin.withStats(BaseStats(10, 10, 10, 10, 10, 10)).withCondition(Stunned(20))

          val dc = 5
          val (result1, _) = savingThrowPassed(dc, Constitution, stunnedGoblin)
          val (result2, _) = savingThrowPassed(dc, Intelligence, stunnedGoblin)
          val (result3, _) = savingThrowPassed(dc, Wisdom, stunnedGoblin)
          val (result4, _) = savingThrowPassed(dc, Charisma, stunnedGoblin)

          result1 shouldBe true
          result2 shouldBe true
          result3 shouldBe true
          result4 shouldBe true
        }
      }
    }

    "use a legendary creature's Legendary Resistance to pass a saving throw despite failing on the roll" in {
      forAll { lich: Lich =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(2)

          val dc = 20
          val (result, updatedLich: Lich) = savingThrowPassed(dc, Constitution, lich)

          result shouldBe true
          updatedLich.legendaryResistances shouldBe 2
        }
      }
    }

    "not use a legendary creature's Legendary Resistance on a pass" in {
      forAll { lich: Lich =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(20)

          val dc = 2
          val (result, updatedLich: Lich) = savingThrowPassed(dc, Constitution, lich)

          result shouldBe true
          updatedLich.legendaryResistances shouldBe 3
        }
      }
    }

    "not use a legendary creature's Legendary Resistance if they have no usages left" in {
      forAll { lich: Lich =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(2)

          val noLegendaryResistancesLich = lich.withNoLegendaryResistancesLeft()

          val dc = 20
          val (result, updatedLich: Lich) = savingThrowPassed(dc, Constitution, noLegendaryResistancesLich)

          result shouldBe false
          updatedLich.legendaryResistances shouldBe 0
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

          val (result, _) = savingThrowWithAdvantagePassed(10, Dexterity, monster)

          result shouldBe true
        }
      }
    }

    "return true if the target's roll exceeds the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          val iterator = Iterator(1, 10)
          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val monster = testMonster.withDexterity(14)

          val (result, _) = savingThrowWithAdvantagePassed(10, Dexterity, monster)

          result shouldBe true
        }
      }
    }

    "return false if the target's roll is less than the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          val iterator = Iterator(1, 2)
          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val monster = testMonster.withDexterity(10)

          val (result, _) = savingThrowWithAdvantagePassed(20, Dexterity, monster)

          result shouldBe false
        }
      }
    }

    "use Legendary Resistance only once when making a saving throw with advantage" in {
      forAll { lich: Lich =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(2)

          val dc = 20
          val (result, updatedLich: Lich) = savingThrowWithAdvantagePassed(dc, Constitution, lich)

          result shouldBe true
          updatedLich.legendaryResistances shouldBe 2
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

          val (result, _) = savingThrowWithDisadvantagePassed(10, Dexterity, monster)

          result shouldBe true
        }
      }
    }

    "return true if the target's roll exceeds the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          val iterator = Iterator(15, 10)
          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val monster = testMonster.withDexterity(14)

          val (result, _) =  savingThrowWithDisadvantagePassed(10, Dexterity, monster)

          result shouldBe true
        }
      }
    }

    "return false if the target's roll is less than the caster's spell save DC" in {
      forAll { testMonster: TestMonster =>
        new TestContext {
          val iterator = Iterator(20, 1)
          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val monster = testMonster.withDexterity(10)

          val (result, _) = savingThrowWithDisadvantagePassed(20, Dexterity, monster)

          result shouldBe false
        }
      }
    }

    "use Legendary Resistance only once when making a saving throw with disadvantage" in {
      forAll { lich: Lich =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(2)

          val dc = 20
          val (result, updatedLich: Lich) = savingThrowWithDisadvantagePassed(dc, Constitution, lich)

          result shouldBe true
          updatedLich.legendaryResistances shouldBe 2
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
