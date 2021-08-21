package io.github.tjheslin1.dmspredictor.classes.fighter

import io.github.tjheslin1.dmspredictor.classes.FightingUtils.duelingFightingStyleConditionsMet
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Weapon.bonusToHitWeapon
import io.github.tjheslin1.dmspredictor.model._
import monocle.Lens

trait BaseFighter extends Player with Product with Serializable {

  val fightingStyles: List[FighterFightingStyle]
  val abilityUsages: BaseFighterAbilities

  def resetStartOfTurn(): Creature = this
}

object BaseFighter {

  val HitDice = D10

  def calculateHealth(level: Level, constitutionScore: Stat): Int = Player.calculateHealth(
    HitDice,
    level,
    constitutionScore)

  def weaponWithFightingStyle[_: RS](
      weapon: Weapon,
      offHand: Option[Equipment],
      fightingStyles: List[FighterFightingStyle]
  ): Weapon =
    weapon.weaponType match {
      case Ranged if fightingStyles.contains(Archery) =>
        bonusToHitWeapon(weapon, 2)
      case Melee if duelingFightingStyleConditionsMet(weapon, offHand, fightingStyles, Dueling) =>
        bonusToHitWeapon(weapon, 2)
      case Melee if weapon.twoHanded && fightingStyles.contains(GreatWeaponFighting) =>
        lazy val rerollingDamage = {
          val damageRoll = weapon.damage
          if (damageRoll <= 2)
            weapon.damage
          else
            damageRoll
        }
        Weapon(
          weapon.name,
          weapon.weaponType,
          weapon.damageType,
          weapon.twoHanded,
          weapon.finesse,
          rerollingDamage,
          weapon.hitBonus
        )
      case _ =>
        weapon
    }

  def armourClassWithFightingStyle(
      stats: BaseStats,
      armour: Armour,
      offHand: Option[Equipment],
      fightingStyles: List[FighterFightingStyle]
  ): Int = {
    val baseArmourClass = armour.armourClass(stats.dexterity)

    val shieldBonus =
      offHand match {
        case Some(Shield) =>
          Shield.armourClass(stats.dexterity)
        case _ =>
          0
      }

    val defenseBonus =
      if (fightingStyles.contains(Defense))
        1
      else
        0

    armour match {
      case NoArmour =>
        baseArmourClass + shieldBonus
      case _ =>
        baseArmourClass + shieldBonus + defenseBonus
    }
  }

  val abilityUsagesLens: Lens[BaseFighter, BaseFighterAbilities] =
    Lens[BaseFighter, BaseFighterAbilities](_.abilityUsages) { abilityUsages =>
      {
        case champion: Champion =>
          Champion._abilityUsages.set(abilityUsages)(champion)
        case fighter: Fighter =>
          Fighter._abilityUsages.set(abilityUsages)(fighter)

        case _ =>
          throw new NotImplementedError("Missing a case in abilityUsagesLens")

      }
    }
}
