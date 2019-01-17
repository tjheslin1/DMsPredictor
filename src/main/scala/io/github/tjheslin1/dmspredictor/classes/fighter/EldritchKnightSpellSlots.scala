package io.github.tjheslin1.dmspredictor.classes.fighter

import io.github.tjheslin1.dmspredictor.model.spellcasting._

case class EldritchKnightSpellSlots(firstLevel: FirstLevelSpellSlot)

object EldritchKnightSpellSlots {

  def highestSpellSlotAvailable(spellSlots: EldritchKnightSpellSlots): SpellSlot = spellSlots.firstLevel

  def available(spellSlots: EldritchKnightSpellSlots): Boolean = spellSlots.firstLevel.count > 0

}
