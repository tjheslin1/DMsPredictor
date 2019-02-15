package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import monocle.Lens
import monocle.macros.GenLens

case class SpellSlots(firstLevel: FirstLevelSpellSlot)

object SpellSlots {

  def highestSpellSlotAvailable(spellSlots: SpellSlots): Option[SpellSlot] =
    if (spellSlots.firstLevel.count > 0) Some(spellSlots.firstLevel) else none[SpellSlot]

  val firstLevelSpellSlotLens: Lens[SpellSlots, FirstLevelSpellSlot] =
    GenLens[SpellSlots](_.firstLevel)

  val firstLevelSpellSlotCountLens: Lens[FirstLevelSpellSlot, Int] =
    GenLens[FirstLevelSpellSlot](_.count)
}
