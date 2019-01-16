package io.github.tjheslin1.dmspredictor.model.spellcasting

sealed trait SpellSlot extends Product with Serializable

case class FirstLevelSpellSlot(count: Int) extends SpellSlot
case class SecondLevelSpellSlot(count: Int) extends SpellSlot
case class ThirdLevelSpellSlot(count: Int) extends SpellSlot
case class FourthLevelSpellSlot(count: Int) extends SpellSlot
