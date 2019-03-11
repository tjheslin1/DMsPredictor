package io.github.tjheslin1.dmspredictor.model.spellcasting

import eu.timepit.refined.auto._

sealed trait SpellSlot extends Product with Serializable {
  val spellLevel: SpellLevel
  val count: Int
}

case class FirstLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 1
}

case class SecondLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 2
}

case class ThirdLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 3
}
