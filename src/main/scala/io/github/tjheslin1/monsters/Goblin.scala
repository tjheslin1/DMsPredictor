package io.github.tjheslin1.monsters

import io.github.tjheslin1.model.{BaseStats, Creature, D6}
import io.github.tjheslin1.util.IntOps._

case class Goblin() extends Creature {

  def stats: BaseStats = BaseStats(8, 14, 10, 10, 8, 8)
  def health: Int      = 2 * D6
  def armourClass: Int = 15
  def experience: Int  = 50
}
