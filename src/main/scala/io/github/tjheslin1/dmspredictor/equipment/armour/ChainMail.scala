package io.github.tjheslin1.dmspredictor.equipment.armour

import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat

case object ChainMail extends Armour {

  val name = "ChainMail"

  def armourClass(dexterity: Stat): Int = 16
}
