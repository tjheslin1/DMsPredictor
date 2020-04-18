package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.Show
import cats.data.NonEmptyList
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighter._
import io.github.tjheslin1.dmspredictor.classes.fighter.Champion._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Champion(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    skills: Skills,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    fightingStyles: List[FighterFightingStyle] = List.empty[FighterFightingStyle],
    abilityUsages: BaseFighterAbilities = BaseFighterAbilities.allUnused,
    proficiencyBonus: ProficiencyBonus = 0,
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List.empty[DamageType],
    damageImmunities: List[DamageType] = List.empty[DamageType],
    conditionResistances: List[Condition] = List.empty[Condition],
    conditionImmunities: List[Condition] = List.empty[Condition],
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    abilities: List[CombatantAbility] = standardChampionAbilities,
    conditions: List[Condition] = List.empty[Condition],
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName
) extends BaseFighter {

  val savingThrowProficiencies = NonEmptyList.of(Strength, Constitution)

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Champion =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))

  def scoresCritical(roll: Int): Boolean = if (level.value <= 2) roll == 20 else roll >= 19

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Champion {

  import BaseFighterAbilities._

  val HitDice = D10

  val standardChampionAbilities: List[CombatantAbility] = List(
    actionSurge(1),
    secondWind(2),
    extraAttack(3),
    twoWeaponFighting(4)
  )

  implicit def championShow[_: RS]: Show[Champion] = Show.show { champion =>
    s"Champion: " +
      s"Name: ${champion.name}, " +
      s"health: ${champion.health}, " +
      s"AC: ${champion.armourClass}"
  }

  val strengthLens: Lens[Champion, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Champion, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Champion, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Champion, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Champion, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Champion, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
