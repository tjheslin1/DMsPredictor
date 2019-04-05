package io.github.tjheslin1.dmspredictor.equipment.weapons

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._

case object Shortsword extends Weapon {

  val name       = "Shortsword"
  val weaponType = Melee
  val damageType = Slashing
  val twoHanded  = false
  val finesse    = true

  def damage(implicit rollStrategy: RollStrategy): Int = 1 * D6

}
