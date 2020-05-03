package unit.conditions

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Paralyzed, Poisoned}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class ParalyzedSpec extends UnitSpecBase {

  "handle" should {
    "sustain the Paralyzed condition if saving throw failed" in {
      new TestContext {
        override implicit val roll: RollStrategy = Dice.naturalOne

        val paralyzed       = Paralyzed(10, 10, Wisdom)
        val poisoned        = Poisoned(10, 10)
        val conditionGoblin = random[Goblin].withWisdom(2).withConditions(paralyzed, poisoned)

        val updatedGoblin = paralyzed.handleEndOfTurn(conditionGoblin)

        updatedGoblin.conditions should contain theSameElementsAs List(paralyzed, poisoned)
        updatedGoblin.defenseStatus shouldBe Disadvantage
      }
    }

    "remove Paralyzed condition if saving throw passed" in {
        new TestContext {
          override implicit val roll = D20.naturalTwenty

          val paralyzed       = Paralyzed(10, 10, Wisdom)
          val poisoned        = Poisoned(10, 10)
          val conditionGoblin = random[Goblin].withWisdom(20).withConditions(paralyzed, poisoned)

          val updatedGoblin = paralyzed.handleEndOfTurn(conditionGoblin)

          updatedGoblin.conditions should contain theSameElementsAs List(poisoned)
          updatedGoblin.defenseStatus shouldBe Regular
      }
    }
  }

  "onConditionApplied" should {

    "set the creature's defense status to Disadvantage" in {
      new TestContext {
        override implicit val roll: RollStrategy = _ => RollResult(10)

        val paralyzed       = Paralyzed(10, 10, Wisdom)

        val goblin = random[Goblin]

        val paralyzedGoblin = paralyzed.onConditionApplied(goblin)

        paralyzedGoblin.defenseStatus shouldBe Disadvantage
      }
    }
  }

  private abstract class TestContext {
    implicit val roll: RollStrategy
  }
}
