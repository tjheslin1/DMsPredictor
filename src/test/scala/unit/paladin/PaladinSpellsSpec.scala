package unit.paladin

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model._

class PaladinSpellsSpec extends UnitSpecBase {

  "Bless" should {
    "add 1d4 to an attack roll" in {
      fail("TODO")
    }

    "add 1d4 to a saving throw roll" in {
      fail("TODO")
    }
  }

  abstract private class TestContext {
    implicit val rollStrategy: RollStrategy
  }
}
