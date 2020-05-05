package unit.conditions

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Grappled, Poisoned}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class GrappledSpec extends UnitSpecBase {

  "handleStartOfTurn" should {
    "remove Grappled condition if passed" in {
      forAll { goblin: Goblin =>
        new TestContext {
          override implicit val roll = D20.naturalTwenty

          val grappled          = Grappled(1)
          val poisoned        = Poisoned(10, 10)
          val conditionGoblin = goblin.withStrength(20).withConditions(grappled, poisoned)

          val updatedGoblin = grappled.handleStartOfTurn(conditionGoblin)

          updatedGoblin.conditions should contain theSameElementsAs List(poisoned)
        }
      }
    }
  }

  private abstract class TestContext {
    implicit val roll: RollStrategy
  }
}
