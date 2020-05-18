package unit.paladin

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class BasePaladinAbilitiesSpec extends UnitSpecBase {

  "Lay on Hands" should {
    "not meet the condition if the Paladin has no points left in its Lay on Hands pool" in {
      fail("TODO")
    }

    "not be triggered if no allies are below half their max hit points" in {
      fail("TODO")
    }


    "not be triggered if no allies are below half their max hit points" in {
      fail("TODO")
    }

    "heal the target for up to their max hit points" in {
      fail("TODO")
    }

    "heal the target using all remaining points in the Lay on Hands pool" in {
      fail("TODO")
    }


    "bring an unconscious ally back to consciousness" in {
      fail("TODO")
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
