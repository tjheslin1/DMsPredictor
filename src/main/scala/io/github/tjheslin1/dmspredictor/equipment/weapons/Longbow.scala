package io.github.tjheslin1.dmspredictor.equipment.weapons

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._

case object Longbow extends Weapon {

  val name: String = "Longbow"
  val weaponType: WeaponType = Ranged
  val damageType: DamageType = Piercing
  val twoHanded: Boolean = true
  val finesse: Boolean = false

  def damage(implicit rollStrategy: RollStrategy): Int = 1 * D8
}
