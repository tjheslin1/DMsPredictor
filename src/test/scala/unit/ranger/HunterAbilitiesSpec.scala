package unit.ranger

import base.{Tracking, UnitSpecBase}
import io.github.tjheslin1.dmspredictor.model._

class HunterAbilitiesSpec extends UnitSpecBase {

  "Colossus Slayer" should {
    "deal 1d8 damage if the enemy is below max health" in {
      fail("TODO")
    }

    "not deal damage if the enemy is at max health" in {
      fail("TODO")
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
