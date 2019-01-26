package io.github.tjheslin1.dmspredictor.equipment.armour

import cats.Show
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat

abstract class Armour {

  val name: String
  def armourClass(dexterity: Stat): Int
}

object Armour {
  implicit val armourShow: Show[Armour] = Show.show { armour =>
    s"Armour: ${armour.name}"
  }
}
