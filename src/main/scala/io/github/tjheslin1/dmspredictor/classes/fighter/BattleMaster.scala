package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.Show
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.BattleMaster.standardBattleMasterAbilities
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class BattleMaster(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    fightingStyles: List[FighterFightingStyle] = List.empty[FighterFightingStyle],
    abilityUsages: BaseFighterAbilities = BaseFighterAbilities.allUnused(),
    superiorityDiceCount: Int = 4,
    proficiencyBonus: ProficiencyBonus = 0,
    resistances: List[DamageType] = List.empty,
    immunities: List[DamageType] = List.empty,
    bonusActionUsed: Boolean = false,
    abilities: List[CombatantAbility] = standardBattleMasterAbilities,
    name: String = NameGenerator.randomName)
    extends BaseFighter {

  import Fighter._

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  def updateHealth(modification: Int): BattleMaster =
    copy(health = Math.max(0, health + modification))

  def scoresCritical(roll: Int): Boolean = roll == 20

//  Maneuver save DC = 8 + your proficiency bonus + your Strength or Dexterity modifier (your choice)
  val maneuverSaveDC: Int = {
    val highestScore = Math.max(stats.strength.value, stats.dexterity.value)

    // unsafe cast to work around usage of runtime variable, already guaranteed to match refinement, see [[BaseStats]]
    8 + proficiencyBonus + Modifier.mod(Refined.unsafeApply(highestScore))
  }
}

object BattleMaster {

  import BaseFighterAbilities._
  import BattleMasterAbilities._

  val HitDice         = D10
  val SuperiorityDice = D8

  /*
    Extra Attack must come before any maneuvers, which will be called by Extra Attack, see [[CoreAbilities]]
   */
  val standardBattleMasterAbilities: List[CombatantAbility] = List(
    actionSurge(1),
    secondWind(2),
    extraAttack(3),
    disarmingAttackManeuver(4),
    twoWeaponFighting(5)
  )

  implicit def battleMasterShow[_: RS]: Show[BattleMaster] = Show.show { battleMaster =>
    s"BattleMaster: " +
      s"Name: ${battleMaster.name}, " +
      s"health: ${battleMaster.health}, " +
      s"AC: ${battleMaster.armourClass}"
  }

  val strengthLens: Lens[BattleMaster, Stat]  = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[BattleMaster, Stat] = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[BattleMaster, Stat] = _stats composeLens GenLens[BaseStats](
    _.constitution)
  val wisdomLens: Lens[BattleMaster, Stat] = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[BattleMaster, Stat] = _stats composeLens GenLens[BaseStats](
    _.intelligence)
  val charismaLens: Lens[BattleMaster, Stat] = _stats composeLens GenLens[BaseStats](_.charisma)
}
