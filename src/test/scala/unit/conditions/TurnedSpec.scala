package unit.conditions

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Poisoned, Turned}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class TurnedSpec extends UnitSpecBase {

  "handle" should {
    "decrement turnsLeft" in {
      forAll { goblin: Goblin =>
        new TestContext {
          implicit override val roll = D20.naturalTwenty

          val turned = Turned(20, 10)

          val updatedGoblin = turned.handle(goblin.withCondition(turned).withWisdom(1))

          updatedGoblin.conditions should contain theSameElementsAs List(Turned(20, 9))
        }
      }
    }

    "remove Turned condition if damage taken" in new TestContext {
      forAll { goblin: Goblin =>
        new TestContext {
          implicit override val roll = D20.naturalTwenty

          val turned          = Turned(1, 10)
          val poisoned        = Poisoned(10, 10)
          val conditionGoblin = goblin.withWisdom(20).withConditions(turned, poisoned)

          val updatedGoblin = turned.handleOnDamage(conditionGoblin)

          updatedGoblin.conditions should contain theSameElementsAs List(poisoned)
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
