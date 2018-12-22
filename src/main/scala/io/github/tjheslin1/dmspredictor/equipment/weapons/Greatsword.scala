package io.github.tjheslin1.dmspredictor.equipment.weapons

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._

case object Greatsword extends Weapon {

  val name       = "Greatsword"
  val weaponType = Melee
  val damageType = Slashing
  val twoHanded  = true

  def damage(implicit rollStrategy: RollStrategy): Int = 2 * D6

}
