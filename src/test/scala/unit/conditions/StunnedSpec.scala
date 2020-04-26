package unit.conditions

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Poisoned, Stunned}
import util.TestData._

class StunnedSpec extends UnitSpecBase {

  "handle" should {

    "sustain Stunned condition if saving throw failed" in {
      forAll { rogue: Rogue =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(2)

          val stunned = Stunned(10)
          val poisoned = Poisoned(10, 10)
          val stunnedRogue = rogue.withConstitution(10).withCondition(stunned, poisoned)

          val updatedRogue = stunned.handleEndOfTurn(stunnedRogue)

          updatedRogue.conditions should contain theSameElementsAs List(stunned, poisoned)
        }
      }
    }

    "set Defense Status to Disadvantage on saving throw failed" in {
      forAll { rogue: Rogue =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(2)

          val stunned = Stunned(10)
          val poisoned = Poisoned(10, 10)
          val stunnedRogue = rogue.withConstitution(10).withCondition(stunned, poisoned)

          val updatedRogue = stunned.handleEndOfTurn(stunnedRogue)

          updatedRogue.defenseStatus shouldBe Disadvantage
        }
      }
    }

    "remove Charmed condition if saving throw passed" in {
      forAll { rogue: Rogue =>
        new TestContext {
          override implicit val roll: RollStrategy = D20.naturalTwenty

          val stunned = Stunned(10)
          val poisoned = Poisoned(10, 10)
          val stunnedRogue = rogue.withConstitution(10).withCondition(stunned, poisoned)

          val updatedRogue = stunned.handleEndOfTurn(stunnedRogue)

          updatedRogue.conditions should contain theSameElementsAs List(poisoned)
        }
      }
    }

    "set Defense Status to to Regular on saving throw passed" in {
      forAll { rogue: Rogue =>
        new TestContext {
          override implicit val roll: RollStrategy = D20.naturalTwenty

          val stunned = Stunned(10)
          val poisoned = Poisoned(10, 10)
          val stunnedRogue = rogue.withConstitution(10).withCondition(stunned, poisoned)

          val updatedRogue = stunned.handleEndOfTurn(stunnedRogue)

          updatedRogue.defenseStatus shouldBe Regular
        }
      }
    }
  }

  private abstract class TestContext {
    implicit val roll: RollStrategy
  }
}
