package io.github.tjheslin1.dmspredictor.model

import cats.Show

sealed trait WeaponType

case object Melee extends WeaponType
case object Ranged extends WeaponType

abstract class Weapon {

  val name: String
  val weaponType: WeaponType
  val damageType: DamageType
  val hitBonus: Int = 0

  def damage(implicit rollStrategy: RollStrategy): Int
}

object Weapon {

  def apply(wpName: String, `type`: WeaponType, dmgType: DamageType, dmg: => Int, wpnHitBonus: Int = 0): Weapon = new Weapon {
    val name: String = wpName
    val weaponType: WeaponType = `type`
    val damageType: DamageType = dmgType
    override val hitBonus: Int = wpnHitBonus

    def damage(implicit rollStrategy: RollStrategy): Int = dmg
  }

  def fixedDamageWeapon[_: RS](weaponName: String, wpnType: WeaponType = Melee, weaponDamageType: DamageType, dmg: Int): Weapon =
    Weapon(weaponName, wpnType, weaponDamageType, dmg)

  implicit val weaponShow: Show[Weapon] = Show.show { weapon =>
    s"Weapon: ${weapon.name} (${weapon.damageType}})"
  }
}
