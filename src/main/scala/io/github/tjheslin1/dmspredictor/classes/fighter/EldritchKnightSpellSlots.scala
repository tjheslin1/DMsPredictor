package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.spellcasting._

case class EldritchKnightSpellSlots(firstLevel: FirstLevelSpellSlot)

object EldritchKnightSpellSlots {

  def highestSpellSlotAvailable(spellSlots: EldritchKnightSpellSlots): Option[SpellSlot] =
    if (spellSlots.firstLevel.count > 0) spellSlots.firstLevel.some else none[SpellSlot]

  def available(spellSlots: EldritchKnightSpellSlots): Boolean = spellSlots.firstLevel.count > 0

}
