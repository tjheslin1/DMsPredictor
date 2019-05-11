package io.github.tjheslin1.dmspredictor.equipment.weapons

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._

case object Greataxe extends Weapon {

  val name       = "Greataxe"
  val weaponType = Melee
  val damageType = Slashing
  val twoHanded  = true
  val finesse    = false

  def damage(implicit rollStrategy: RollStrategy): Int = 1 * D12

}
