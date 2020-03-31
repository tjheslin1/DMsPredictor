package unit.ranger

import base.{Tracking, UnitSpecBase}
import io.github.tjheslin1.dmspredictor.model._

class HunterAbilitiesSpec extends UnitSpecBase {

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
