package io.github.tjheslin1.dmspredictor.equipment.weapons

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._

case object PlusOneShortsword extends Weapon {

  val name       = "Shortsword +1"
  val weaponType = Melee
  val damageType = Magical
  val twoHanded  = false
  val finesse    = true

  override val hitBonus = 1

  def damage(implicit rollStrategy: RollStrategy): Int = 1 * D6

}
