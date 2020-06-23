package unit.paladin

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model._

class PaladinSpellsSpec extends UnitSpecBase {

  "Bless" should {
    "prioritise buffing melee classes" in {
      fail("TODO")
    }
  }

  abstract private class TestContext {
    implicit val rollStrategy: RollStrategy
  }
}
