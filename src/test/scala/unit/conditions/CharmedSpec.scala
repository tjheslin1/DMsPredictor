package unit.conditions

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class CharmedSpec extends UnitSpecBase {

  "handle" should {
    "sustain Charmed condition if saving throw failed" in {
      forAll { goblin: Goblin =>
        new TestContext {
          override implic
          it val roll = _ => RollResult(2)

          val charmed         = Charmed(15)
          val poisoned        = Poisoned(10, 10)
          val conditionGoblin = goblin.withWisdom(20).withConditions(charmed, poisoned)

          val updatedGoblin = charmed.handleStartOfTurn(conditionGoblin)

          updatedGoblin.conditions should contain theSameElementsAs List(charmed, poisoned)
        }
      }
    }

    "remove Charmed condition if saving throw passed and apply VampireCharmImmunity" in {
      forAll { goblin: Goblin =>
        new TestContext {
          override implicit val roll = D20.naturalTwenty

          val charmed         = Charmed(1)
          val poisoned        = Poisoned(10, 10)
          val conditionGoblin = goblin.withWisdom(20).withConditions(charmed, poisoned)

          val updatedGoblin = charmed.handleStartOfTurn(conditionGoblin)

          updatedGoblin.conditions should contain theSameElementsAs List(poisoned, VampireCharmImmunity)
        }
      }
    }
  }

  private abstract class TestContext {
    implicit val roll: RollStrategy
  }
}
