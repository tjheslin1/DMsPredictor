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
  val finesse: Boolean

  def damage(implicit rollStrategy: RollStrategy): Int
}

object Weapon {

  def apply(
      wpName: String,
      `type`: WeaponType,
      dmgType: DamageType,
      isTwoHanded: Boolean,
      isFinesse: Boolean,
      dmg: => Int,
      wpnHitBonus: Int = 0
  ): Weapon =
    new Weapon {
      val name: String           = wpName
      val weaponType: WeaponType = `type`
      val damageType: DamageType = dmgType
      override val hitBonus: Int = wpnHitBonus
      val twoHanded              = isTwoHanded
      val finesse                = isFinesse

      def damage(implicit rollStrategy: RollStrategy): Int = dmg
    }

  def ofDamageTypeWeapon[_: RS](weapon: Weapon, damageType: DamageType): Weapon = Weapon(
    weapon.name,
    weapon.weaponType,
    damageType,
    weapon.twoHanded,
    weapon.finesse,
    weapon.damage,
    weapon.hitBonus
  )

  def bonusToHitWeapon[_: RS](weapon: Weapon, bonus: Int): Weapon = Weapon(
    weapon.name,
    weapon.weaponType,
    weapon.damageType,
    weapon.twoHanded,
    weapon.finesse,
    weapon.damage,
    weapon.hitBonus + bonus
  )

  def bonusDamageWeapon[_: RS](weapon: Weapon, bonusDamage: => Int): Weapon = Weapon(
    weapon.name,
    weapon.weaponType,
    weapon.damageType,
    weapon.twoHanded,
    weapon.finesse,
    weapon.damage + bonusDamage,
    weapon.hitBonus
  )

  def fixedDamageWeapon[_: RS](
      weaponName: String,
      wpnType: WeaponType = Melee,
      weaponDamageType: DamageType,
      twoHands: Boolean,
      finesse: Boolean,
      dmg: Int
  ): Weapon = Weapon(weaponName, wpnType, weaponDamageType, twoHands, finesse, dmg)

  case class UnarmedStrike(creature: Creature) extends Weapon {
    override val name: String           = "Unarmed strike"
    override val weaponType: WeaponType = Melee
    override val damageType: DamageType = Bludgeoning
    override val twoHanded: Boolean     = false
    override val finesse: Boolean       = false

    override def damage(implicit rollStrategy: RollStrategy): Int =
      1 + Modifier.mod(creature.stats.strength)
  }

  implicit val weaponShow: Show[Weapon] = Show.show { weapon =>
    s"Weapon: ${weapon.name} (${weapon.damageType})"
  }
}
