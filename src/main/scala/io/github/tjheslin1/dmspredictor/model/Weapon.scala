package io.github.tjheslin1.dmspredictor.model

import cats.Show
import io.github.tjheslin1.dmspredictor.equipment.Equipment

sealed trait WeaponType

case object Melee  extends WeaponType
case object Ranged extends WeaponType

abstract class Weapon extends Equipment {

  val name: String
  val weaponType: WeaponType
  val damageType: DamageType
  val hitBonus: Int = 0
  val twoHanded: Boolean

  def damage(implicit rollStrategy: RollStrategy): Int
}

object Weapon {

  def apply(wpName: String,
            `type`: WeaponType,
            dmgType: DamageType,
            twoHands: Boolean,
            dmg: => Int,
            wpnHitBonus: Int = 0): Weapon =
    new Weapon {
      val name: String           = wpName
      val weaponType: WeaponType = `type`
      val damageType: DamageType = dmgType
      override val hitBonus: Int = wpnHitBonus
      val twoHanded              = twoHands

      def damage(implicit rollStrategy: RollStrategy): Int = dmg
    }

  def bonusToHitWeapon[_: RS](weapon: Weapon, bonus: Int): Weapon =
    Weapon(weapon.name, weapon.weaponType, weapon.damageType, weapon.twoHanded, weapon.damage, weapon.hitBonus + bonus)

  def fixedDamageWeapon[_: RS](weaponName: String,
                               wpnType: WeaponType = Melee,
                               weaponDamageType: DamageType,
                               twoHands: Boolean,
                               dmg: Int): Weapon =
    Weapon(weaponName, wpnType, weaponDamageType, twoHands, dmg)

  case class UnarmedStrike(creature: Creature) extends Weapon {
    override val name: String           = "Unarmed strike"
    override val weaponType: WeaponType = Melee
    override val damageType: DamageType = Bludgeoning
    override val twoHanded: Boolean     = false

    override def damage(implicit rollStrategy: RollStrategy): Int = 1 + Modifier.mod(creature.stats.strength)
  }

  implicit val weaponShow: Show[Weapon] = Show.show { weapon =>
    s"Weapon: ${weapon.name} (${weapon.damageType}})"
  }
}
