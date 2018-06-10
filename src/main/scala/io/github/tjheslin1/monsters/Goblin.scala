package io.github.tjheslin1.monsters

import io.github.tjheslin1.model.{BaseStats, Creature, D6, RollStrategy}
import io.github.tjheslin1.util.IntOps._

case class Goblin() extends Creature {

  def health(implicit rollStrategy: RollStrategy): Int = 2 * D6

  def stats: BaseStats = BaseStats(8, 14, 10, 10, 8, 8)
  def armourClass: Int = 15
  def experience: Int  = 50
}
