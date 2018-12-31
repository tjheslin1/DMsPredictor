package io.github.tjheslin1.dmspredictor.classes.fighter

import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighter._
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighterAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter.{armourClassWithFightingStyle, weaponWithFightingStyle}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.Weapon.bonusToHitWeapon
import io.github.tjheslin1.dmspredictor.model._

abstract class BaseFighter extends Creature {

  val level: Level
  val health: Int
  val maxHealth: Int
  val stats: BaseStats
  val baseWeapon: Weapon
  val armour: Armour
  val offHand: Option[Equipment]
  val fightingStyles: List[FighterFightingStyle]
  val abilityUsages: BaseFighterAbilities

  val creatureType: CreatureType = PlayerCharacter

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  val abilities: List[CreatureAbility[BaseFighter]] = baseFighterAbilities
}

object BaseFighter {

  val HitDice = D10

  val baseFighterAbilities: List[CreatureAbility[BaseFighter]] = List(
    1 -> secondWind,
    2 -> actionSurge,
    3 -> twoWeaponFighting,
  )

  def weaponWithFightingStyle[_: RS](weapon: Weapon, fightingStyles: List[FighterFightingStyle]) =
    weapon.weaponType match {
      case Ranged if fightingStyles.contains(Archery) =>
        bonusToHitWeapon(weapon, 2)
      case Melee if weapon.twoHanded == false && fightingStyles.contains(Dueling) =>
        bonusToHitWeapon(weapon, 2)
      case Melee if weapon.twoHanded && fightingStyles.contains(GreatWeaponFighting) =>
        lazy val rerollingDamage = {
          val damageRoll = weapon.damage
          if (damageRoll <= 2)
            weapon.damage
          else
            damageRoll
        }
        Weapon(weapon.name, weapon.weaponType, weapon.damageType, weapon.twoHanded, rerollingDamage, weapon.hitBonus)
      case _ => weapon
    }

  def armourClassWithFightingStyle(stats: BaseStats,
                                   armour: Armour,
                                   offHand: Option[Equipment],
                                   fightingStyles: List[FighterFightingStyle]) = {
    val baseArmourClass = armour.armourClass(stats.dexterity)
    val shieldBonus = offHand match {
      case Some(Shield()) => 2
      case _              => 0
    }
    val defenseBonus = if (fightingStyles.contains(Defense)) 1 else 0

    armour match {
      case NoArmour => baseArmourClass + shieldBonus
      case _        => baseArmourClass + shieldBonus + defenseBonus
    }
  }

}
