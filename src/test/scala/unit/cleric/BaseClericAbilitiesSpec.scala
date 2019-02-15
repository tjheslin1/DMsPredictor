package unit.cleric

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.BaseClericAbilities._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._
import util.TestMonster

class BaseClericAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "Sacred Flame" should {

    "deal 1d8 damage from levels one to four" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
    new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(1)

      val combatant = cleric
        .withLevel(LevelOne)
        .withWisdom(20)
        .withAbilities(List(sacredFlame(1)))
        .withCombatIndex(1)

      val lowDexMonster = testMonster.withDexterity(1).withHealth(10).withCombatIndex(2)

      val (_, Some(Combatant(_, updatedMonster: TestMonster))) =
        sacredFlame(Priority)(combatant).useAbility(lowDexMonster.some)

      updatedMonster.health shouldBe 9
    }
  }
    }

    "deal 2d8 damage for level five" in new TestContext {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(1)

          val combatant = cleric
            .withLevel(LevelFive)
            .withWisdom(20)
            .withAbilities(List(sacredFlame(1)))
            .withCombatIndex(1)

          val lowDexMonster = testMonster.withDexterity(1).withHealth(10).withCombatIndex(2)

          val (_, Some(Combatant(_, updatedMonster: TestMonster))) =
            sacredFlame(Priority)(combatant).useAbility(lowDexMonster.some)

          updatedMonster.health shouldBe 8
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
