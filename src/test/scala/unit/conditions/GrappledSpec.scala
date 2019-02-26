package unit.conditions

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Grappled, Poisoned, Turned}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class GrappledSpec extends UnitSpecBase {

  "handle" should {
    "remove Grappled condition if passed" in new TestContext {
      forAll { goblin: Goblin =>
        new TestContext {
          implicit override val roll = D20.naturalTwenty

          val grappled          = Grappled(1)
          val poisoned        = Poisoned(10, 10)
          val conditionGolbin = goblin.withStrength(20).withConditions(grappled, poisoned)

          val updatedGoblin = grappled.handle(conditionGolbin)

          updatedGoblin.conditions should contain theSameElementsAs List(poisoned)
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
