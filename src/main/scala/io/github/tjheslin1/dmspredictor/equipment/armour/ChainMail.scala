package io.github.tjheslin1.dmspredictor.equipment.armour

import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat

case object ChainMail extends Armour {

  val name = "Chain Mail"

  def armourClass(dexterity: Stat): Int = 16
}
