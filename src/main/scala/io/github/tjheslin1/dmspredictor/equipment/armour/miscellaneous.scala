package io.github.tjheslin1.dmspredictor.equipment.armour

import io.github.tjheslin1.dmspredictor.model.Armour
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat

case class Shield() extends Armour {

  val name: String = "Shield"

  def armourClass(dexterity: Stat): Int = 2
}
