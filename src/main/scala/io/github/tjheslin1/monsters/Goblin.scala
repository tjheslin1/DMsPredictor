package io.github.tjheslin1.monsters

import io.github.tjheslin1.model.{BaseStats, D6, Level, LevelOne, Monster, RollStrategy, Weapon}
import io.github.tjheslin1.util.IntOps._
import io.github.tjheslin1.util.NameGenerator
import io.github.tjheslin1.weapons.Shortsword

case class Goblin() extends Monster {

  def calculateHealth(implicit rollStrategy: RollStrategy): Int = 2 * D6

  def stats: BaseStats = BaseStats(8, 14, 10, 10, 8, 8)
  val armourClass: Int = 15
  val experience: Int  = 50

  val weapon: Weapon = Shortsword
  val level: Level = LevelOne
}
