package unit.paladin

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model._

class PaladinSpellsSpec extends UnitSpecBase {

  abstract private class TestContext {
    implicit val rollStrategy: RollStrategy
  }
}
