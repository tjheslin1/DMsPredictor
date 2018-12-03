package io.github.tjheslin1.dmspredictor.model

import cats.Show

abstract class Weapon {

  val name: String
  val damageType: DamageType

  def damage(implicit rollStrategy: RollStrategy): Int
}

object Weapon {

  def fixedDamageWeapon(weaponName: String, weaponDamageType: DamageType, dmg: Int) = new Weapon {
    val name: String = weaponName
    val damageType   = weaponDamageType

    def damage(implicit rollStrategy: RollStrategy): Int = dmg
  }

  implicit val weaponShow: Show[Weapon] = Show.show { weapon =>
    s"Weapon: ${weapon.name} (${weapon.damageType}})"
  }
}
