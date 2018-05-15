package io.github.tjheslin1.classes

import io.github.tjheslin1.model._
import io.github.tjheslin1.weapons.Greatsword

case class Fighter() extends PlayerCharacter {

  def level: Level = LevelOne
  def stats: BaseStats = BaseStats(15, 13, 14, 12, 8, 10)
  def health: Int = D10.max
  def armourClass: Int = 14
  def experience: Int = 0
  def weapon: Weapon = Greatsword
}
