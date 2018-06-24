package io.github.tjheslin1.classes

import io.github.tjheslin1.model._
import io.github.tjheslin1.weapons.Greatsword

class Fighter(wpn: Weapon = Greatsword) extends PlayerCharacter {

  val level: Level                                     = LevelOne
  val stats: BaseStats                                 = BaseStats(15, 13, 14, 12, 8, 10)
  def calculateHealth(implicit rollStrategy: RollStrategy): Int = D10.max * level
  val armourClass: Int                                 = 14
  val experience: Int                                  = 0
  val weapon: Weapon                                   = wpn
}

object Fighter {
  def apply(wpn: Weapon = Greatsword): Fighter = new Fighter(wpn)
}
