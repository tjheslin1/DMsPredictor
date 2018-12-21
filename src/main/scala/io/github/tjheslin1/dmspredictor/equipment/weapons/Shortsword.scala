package io.github.tjheslin1.dmspredictor.equipment.weapons

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._

case object Shortsword extends Weapon {

  val name       = "Shortsword"
  val damageType = Slashing

  def damage(implicit rollStrategy: RollStrategy): Int = 1 * D6

}
