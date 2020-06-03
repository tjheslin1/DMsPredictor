package unit.paladin

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model._

class PaladinSpellsSpec extends UnitSpecBase {

  "Bless" should {
    "have all classes listed in its priority list" in {
      fail("TODO")
    }

    "prioritise buffing melee classes" in {
      fail("TODO")
    }
  }

  abstract private class TestContext {
    implicit val rollStrategy: RollStrategy
  }
}
