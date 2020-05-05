package unit.conditions

import base.{Tracking, UnitSpecBase}
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition._
import util.TestData._

class ConditionSpec extends UnitSpecBase {

  "addCondition" should {

    "add the condition to a creatures list of conditions" in {
      new TestContext {
        implicit override val roll: RollStrategy = Dice.defaultRandomiser

        val fighter = random[Fighter].withCombatIndex(1)

        val condition = trackedCondition(10, true)

        val Combatant(_, updatedFighter: Fighter) = addCondition(fighter, condition)

        updatedFighter.conditions should contain theSameElementsAs List(condition)
      }
    }

    "apply onConditionApplied to creature" in {
      new TestContext {
        implicit override val roll: RollStrategy = Dice.defaultRandomiser

        val fighter = random[Fighter].withCombatIndex(1)

        val defenseDisadvantageCondition =
          trackedCondition(10,
                           true,
                           onApplied = c => Creature.creatureDefenseStatusLens.set(Disadvantage)(c))

        val Combatant(_, updatedFighter: Fighter) =
          addCondition(fighter, defenseDisadvantageCondition)

        updatedFighter.defenseStatus shouldBe Disadvantage
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
