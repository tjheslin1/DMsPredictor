package unit.spellcasting

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots._
import io.github.tjheslin1.dmspredictor.model.spellcasting.{SpellSlots, _}
import org.scalatest.OptionValues

class SpellSlotsSpec extends UnitSpecBase with OptionValues {

  "highestSpellSlotAvailable" should {
    "return first level spell slot when only first level is available" in {
      val slots = SpellSlots(1, 0, 0, 0, 0, 0, 0, 0, 0)

      highestSpellSlotAvailable(slots).value shouldBe FirstLevelSpellSlots(1)
    }

    "highest level spell slot of third" in {
      val slots = SpellSlots(2, 2, 1, 0, 0, 0, 0, 0, 0)

      highestSpellSlotAvailable(slots).value shouldBe ThirdLevelSpellSlots(1)
    }

    "highest level spell slot of ninth" in {
      val slots = SpellSlots(2, 2, 1, 1, 1, 1, 1, 1, 1)

      highestSpellSlotAvailable(slots).value shouldBe NinthLevelSpellSlots(1)
    }
  }
}
