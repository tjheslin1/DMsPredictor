package io.github.tjheslin1.dmspredictor.equipment.armour

import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod

case object ChainShirt extends Armour {

  val name = "ChainShirt"

  def armourClass(dexterity: Stat): Int = 13 + Math.min(2, mod(dexterity))
}
