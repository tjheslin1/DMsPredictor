package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter.EldritchKnightSpellSlots
import io.github.tjheslin1.dmspredictor.model.spellcasting._

class EldritchKnightSpellSlotsSpec extends UnitSpecBase {

  import EldritchKnightSpellSlots._

  "highestSpellSlotAvailable" should {
    "return first level spell slot when only first level is available" in {
      val slots = EldritchKnightSpellSlots(FirstLevelSpellSlot(1))

      highestSpellSlotAvailable(slots) shouldBe FirstLevelSpellSlot(1)
    }
  }
}
