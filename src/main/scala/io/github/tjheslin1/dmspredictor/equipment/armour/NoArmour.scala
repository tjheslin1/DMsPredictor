package io.github.tjheslin1.dmspredictor.equipment.armour

import io.github.tjheslin1.dmspredictor.model.{Armour, Modifier}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat

case object NoArmour extends Armour {

  val name: String = "No armour"

  def armourClass(dexterity: Stat): Int = 10 + Modifier.mod(dexterity)
}
