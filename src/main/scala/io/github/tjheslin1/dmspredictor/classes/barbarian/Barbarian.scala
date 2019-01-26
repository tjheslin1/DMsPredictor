package io.github.tjheslin1.dmspredictor.classes.barbarian

import cats.Show
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Barbarian(level: Level,
                                  health: Int,
                                  maxHealth: Int,
                                  stats: BaseStats,
                                  baseWeapon: Weapon,
                                  rageUsages: Int,
                                  armour: Armour = NoArmour,
                                  offHand: Option[Equipment] = None,
                                  proficiencyBonus: ProficiencyBonus = 0,
                                  resistances: List[DamageType] = List.empty,
                                  immunities: List[DamageType] = List.empty,
                                  bonusActionUsed: Boolean = false,
                                  abilities: List[CombatantAbility] = List.empty,
                                  inRage: Boolean = false,
                                  name: String = NameGenerator.randomName)
    extends BaseBarbarian {

  def weapon[_: RS]: Weapon = weaponWithRageDamage(baseWeapon, inRage)

  val armourClass: Int = calculateArmourClass(stats, armour, offHand)

  def updateHealth(modification: Int): Creature = copy(health = Math.max(0, health + modification))

  def scoresCritical(roll: Int): Boolean = roll == 20
}

object Barbarian {

  val HitDice = D12

  val rageUsagesPerLevel: Map[Level, Int] = Map(
    LevelOne -> 2,
    LevelTwo -> 2,
    LevelThree -> 3,
    LevelFour -> 3,
    LevelFive -> 3
  )

  def calculateHealth[_: RS](level: Level, constitutionScore: Stat): Int =
    (D12.max + mod(constitutionScore)) + ((level.value - 1) * (Dice.midpointRoundedUp(HitDice) + mod(
      constitutionScore)))

  def weaponWithRageDamage[_: RS](weapon: Weapon, inRage: Boolean): Weapon = {
    lazy val inRageDamage = if (inRage) weapon.damage + 2 else weapon.damage

    Weapon(weapon.name,
           weapon.weaponType,
           weapon.damageType,
           weapon.twoHanded,
           inRageDamage,
           weapon.hitBonus)
  }

  def calculateArmourClass(stats: BaseStats, armour: Armour, offHand: Option[Equipment]): Int = {
    val baseArmourClass = armour.armourClass(stats.dexterity)
    val shieldBonus = offHand match {
      case Some(Shield) => Shield.armourClass(stats.dexterity)
      case _            => 0
    }

    armour match {
      case NoArmour => baseArmourClass + mod(stats.constitution) + shieldBonus
      case _        => baseArmourClass + shieldBonus
    }
  }

  implicit def barbarianShow[_: RS]: Show[Barbarian] = Show.show { barbarian =>
    s"Barbarian: " +
      s"Name: ${barbarian.name}, " +
      s"health: ${barbarian.health}, " +
      s"AC: ${barbarian.armourClass}"
  }

  val strengthLens: Lens[Barbarian, Stat]  = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Barbarian, Stat] = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Barbarian, Stat] = _stats composeLens GenLens[BaseStats](
    _.constitution)
  val wisdomLens: Lens[Barbarian, Stat] = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Barbarian, Stat] = _stats composeLens GenLens[BaseStats](
    _.intelligence)
  val charismaLens: Lens[Barbarian, Stat] = _stats composeLens GenLens[BaseStats](_.charisma)
}
