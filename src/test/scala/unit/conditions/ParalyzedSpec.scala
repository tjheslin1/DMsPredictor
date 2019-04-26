package unit.conditions

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Paralyzed, Poisoned}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class ParalyzedSpec extends UnitSpecBase {

  "handle" should {
    "remove Charmed condition if passed" in {
      forAll { goblin: Goblin =>
        new TestContext {
          override implicit val roll = D20.naturalTwenty

          val paralyzed       = Paralyzed(10, 10, Wisdom)
          val poisoned        = Poisoned(10, 10)
          val conditionGoblin = goblin.withWisdom(20).withConditions(paralyzed, poisoned)

          val updatedGoblin = paralyzed.handleEndOfTurn(conditionGoblin)

          updatedGoblin.conditions should contain theSameElementsAs List(poisoned)
        }
      }
    }
  }

  private abstract class TestContext {
    implicit val roll: RollStrategy
  }
}
