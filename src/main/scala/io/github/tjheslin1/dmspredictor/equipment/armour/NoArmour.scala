package io.github.tjheslin1.dmspredictor.equipment.armour

import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._

case object NoArmour extends Armour {

  val name: String = "NoArmour"

  def armourClass(dexterity: Stat): Int = 10 + Modifier.mod(dexterity)
}
