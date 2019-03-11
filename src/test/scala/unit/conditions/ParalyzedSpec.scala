package unit.conditions

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Paralyzed, Poisoned}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class ParalyzedSpec extends UnitSpecBase {

  "handle" should {
    "remove Charmed condition if passed" in new TestContext {
      forAll { goblin: Goblin =>
        new TestContext {
          implicit override val roll = D20.naturalTwenty

          val paralyzed       = Paralyzed(10, 10, Wisdom)
          val poisoned        = Poisoned(10, 10)
          val conditionGoblin = goblin.withWisdom(20).withConditions(paralyzed, poisoned)

          val updatedGoblin = paralyzed.handle(conditionGoblin)

          updatedGoblin.conditions should contain theSameElementsAs List(poisoned)
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
