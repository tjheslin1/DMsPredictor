package unit.spellcasting

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import org.scalatest.OptionValues

class SpellSlotsSpec extends UnitSpecBase with OptionValues {

  "highestSpellSlotAvailable" should {
    "return first level spell slot when only first level is available" in {
      val slots = SpellSlots(FirstLevelSpellSlots(1), SecondLevelSpellSlots(0), ThirdLevelSpellSlots(0))

      highestSpellSlotAvailable(slots).value shouldBe FirstLevelSpellSlots(1)
    }

    "highest level spell slot" in {
      val slots = SpellSlots(FirstLevelSpellSlots(2), SecondLevelSpellSlots(2), ThirdLevelSpellSlots(1))

      highestSpellSlotAvailable(slots).value shouldBe ThirdLevelSpellSlots(1)
    }
  }
}
