package io.github.tjheslin1.dmspredictor.classes.paladin

import io.github.tjheslin1.dmspredictor.classes.FightingUtils.duelingFightingStyleConditionsMet
import io.github.tjheslin1.dmspredictor.classes.paladin.BasePaladinAbilities.SacredWeaponCondition
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Weapon._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import monocle.Lens

trait BasePaladin extends Player with SpellCaster with Product with Serializable {

  val layOnHandsPool: Int

  val fightingStyles: List[PaladinFightingStyle]

  override val cantrip: Option[Spell] = None

  val spellCastingModifier  = proficiencyBonus.value
  val spellCastingAttribute = Charisma

  val spellCastingLevel        = level
  val levelSpellcastingLearned = LevelTwo

  val channelDivinityUsed: Boolean

  def resetStartOfTurn(): Creature = this
}

object BasePaladin {

  val HitDice = D10

  def calculateHealth(level: Level, constitutionScore: Stat): Int = Player.calculateHealth(
    HitDice,
    level,
    constitutionScore)

  def paladinWeapon[_: RS](
      paladin: BasePaladin,
      weapon: Weapon,
      offHand: Option[Equipment]
  ): Weapon = {
    val weaponWithFightingStyle =
      weapon.weaponType match {
        case Melee
            if duelingFightingStyleConditionsMet(
              weapon,
              offHand,
              paladin.fightingStyles,
              Dueling) =>
          bonusToHitWeapon(weapon, 2)
        case Melee if weapon.twoHanded && paladin.fightingStyles.contains(GreatWeaponFighting) =>
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
        case _ => weapon
      }

    val sacredWeaponBuffActive = paladin.conditions.exists {
      case SacredWeaponCondition(_) => true
      case _                        => false
    }

    if (sacredWeaponBuffActive) {
      val charismaBonus = Math.max(1, Modifier.mod(paladin.stats.charisma))

      val bonusToHitWpn = bonusToHitWeapon(weaponWithFightingStyle, charismaBonus)

      ofDamageTypeWeapon(bonusToHitWpn, Magical)
    } else
      weaponWithFightingStyle
  }

  def armourClassWithFightingStyle(
      stats: BaseStats,
      armour: Armour,
      offHand: Option[Equipment],
      fightingStyles: List[PaladinFightingStyle]
  ): Int = {
    val baseArmourClass = armour.armourClass(stats.dexterity)

    val shieldBonus =
      offHand match {
        case Some(Shield) => Shield.armourClass(stats.dexterity)
        case _            => 0
      }

    val defenseBonus =
      if (fightingStyles.contains(Defense))
        1
      else
        0

    armour match {
      case NoArmour => baseArmourClass + shieldBonus
      case _        => baseArmourClass + shieldBonus + defenseBonus
    }
  }

  // format: off
  def paladinSpellSlots(level: Level): SpellSlots =
    level match {
      case LevelOne       => SpellSlots(0, 0, 0)
      case LevelTwo       => SpellSlots(2, 0, 0)
      case LevelThree     => SpellSlots(3, 0, 0)
      case LevelFour      => SpellSlots(3, 0, 0)
      case LevelFive      => SpellSlots(4, 2, 0)
      case LevelSix       => SpellSlots(4, 2, 0)
      case LevelSeven     => SpellSlots(4, 3, 0)
      case LevelEight     => SpellSlots(4, 3, 0)
      case LevelNine      => SpellSlots(4, 3, 2)
      case LevelTen       => SpellSlots(4, 3, 2)
      case LevelEleven    => SpellSlots(4, 3, 3)
      case LevelTwelve    => SpellSlots(4, 3, 3)
      case LevelThirteen  => SpellSlots(4, 3, 3, 1, 0, 0, 0, 0, 0)
      case LevelFourteen  => SpellSlots(4, 3, 3, 1, 0, 0, 0, 0, 0)
      case LevelFifteen   => SpellSlots(4, 3, 3, 2, 0, 0, 0, 0, 0)
      case LevelSixteen   => SpellSlots(4, 3, 3, 2, 0, 0, 0, 0, 0)
      case LevelSeventeen => SpellSlots(4, 3, 3, 3, 1, 0, 0, 0, 0)
      case LevelEighteen  => SpellSlots(4, 3, 3, 3, 1, 0, 0, 0, 0)
      case LevelNineteen  => SpellSlots(4, 3, 3, 3, 2, 0, 0, 0, 0)
      case LevelTwenty    => SpellSlots(4, 3, 3, 3, 2, 0, 0, 0, 0)
    }
  // format: on

  def layOnHandsPoolForLevel(level: Level): Int = level.value * 5

  val layOnHandsPoolLens: Lens[BasePaladin, Int] =
    Lens[BasePaladin, Int](_.layOnHandsPool) { pool =>
      {
        case paladin: Paladin => Paladin._layOnHandsPool.set(pool)(paladin)

        case _ => throw new NotImplementedError("Missing a case in layOnHandsPoolLens")
      }
    }

  val channelDivinityUsedLens: Lens[BasePaladin, Boolean] =
    Lens[BasePaladin, Boolean](_.channelDivinityUsed) { channelDivinityUsed =>
      {
        case paladin: Paladin => Paladin._channelDivinityUsed.set(channelDivinityUsed)(paladin)

        case _ => throw new NotImplementedError("Missing a case in channelDivinityUsedLens")

      }
    }
}
