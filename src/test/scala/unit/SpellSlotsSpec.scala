package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import org.scalatest.OptionValues

class SpellSlotsSpec extends UnitSpecBase with OptionValues {

  "highestSpellSlotAvailable" should {
    "return first level spell slot when only first level is available" in {
      val slots = SpellSlots(FirstLevelSpellSlot(1))

      highestSpellSlotAvailable(slots).value shouldBe FirstLevelSpellSlot(1)
    }
  }
}
