package io.github.tjheslin1.dmspredictor.model.spellcasting

import eu.timepit.refined.auto._

sealed trait SpellSlot extends Product with Serializable {
  val spellLevel: SpellLevel
  val count: Int
}

case class FirstLevelSpellSlot(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 1
}
