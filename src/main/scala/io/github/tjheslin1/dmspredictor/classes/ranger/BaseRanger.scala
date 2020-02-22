package io.github.tjheslin1.dmspredictor.classes.ranger

import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Weapon.bonusToHitWeapon
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell

trait BaseRanger extends Player with SpellCaster {

  val fightingStyles: List[RangerFightingStyle]

  override val cantrip: Option[Spell] = None

  val levelSpellcastingLearned = LevelTwo

  def resetStartOfTurn(): Creature = this
}

object BaseRanger {

  val HitDice = D10

  def calculateHealth(level: Level, constitutionScore: Stat): Int =
    Player.calculateHealth(HitDice, level, constitutionScore)

  def weaponWithFightingStyle[_: RS](
      weapon: Weapon,
      fightingStyles: List[RangerFightingStyle]
  ): Weapon =
    weapon.weaponType match {
      case Ranged if fightingStyles.contains(Archery) =>
        bonusToHitWeapon(weapon, 2)
      case Melee if weapon.twoHanded == false && fightingStyles.contains(Dueling) =>
        bonusToHitWeapon(weapon, 2)
      case _ => weapon
    }

  def armourClassWithFightingStyle(
      stats: BaseStats,
      armour: Armour,
      offHand: Option[Equipment],
      fightingStyles: List[RangerFightingStyle]
  ): Int = {
    val baseArmourClass = armour.armourClass(stats.dexterity)

    val shieldBonus = offHand match {
      case Some(Shield) => Shield.armourClass(stats.dexterity)
      case _            => 0
    }

    val defenseBonus = if (fightingStyles.contains(Defense)) 1 else 0

    armour match {
      case NoArmour => baseArmourClass + shieldBonus
      case _        => baseArmourClass + shieldBonus + defenseBonus
    }
  }
}
