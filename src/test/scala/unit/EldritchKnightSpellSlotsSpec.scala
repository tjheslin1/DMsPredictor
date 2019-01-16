package unit

import base.UnitSpecBase
import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.fighter.EldritchKnightSpellSlots
import io.github.tjheslin1.dmspredictor.model.spellcasting._

class EldritchKnightSpellSlotsSpec extends UnitSpecBase {

  import EldritchKnightSpellSlots._

  "highestSpellSlotAvailable" should {
    "return None when no spell slots are available" in {
      val slots = EldritchKnightSpellSlots(FirstLevelSpellSlot(0))

      highestSpellSlotAvailable(slots) shouldBe none[SpellSlot]
    }
  }
}
