package unit.conditions

import base.{Tracking, UnitSpecBase}
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition._

class ConditionSpec extends UnitSpecBase {

  "addCondition" should {

    "add the condition to a creatures list of conditions" in {
      new TestContext {
        implicit override val roll: RollStrategy = Dice.defaultRandomiser

        val fighter = random[Fighter]

        val condition = trackedCondition(10, true)

        val updatedFighter = addCondition(fighter, condition).asInstanceOf[Fighter]

        updatedFighter.conditions should contain theSameElementsAs List(condition)
      }
    }

    "apply onConditionApplied to creature" in {
      new TestContext {
        implicit override val roll: RollStrategy = Dice.defaultRandomiser

        val fighter = random[Fighter]

        val defenseDisadvantageCondition =
          trackedCondition(10,
                           true,
                           onApplied = c => Creature.creatureDefenseStatusLens.set(Disadvantage)(c))

        val updatedFighter =
          addCondition(fighter, defenseDisadvantageCondition).asInstanceOf[Fighter]

        updatedFighter.defenseStatus shouldBe Disadvantage
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
