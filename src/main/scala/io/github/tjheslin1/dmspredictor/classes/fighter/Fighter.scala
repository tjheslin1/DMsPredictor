package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.Show
import cats.data.NonEmptyList
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighter._
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighterAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter.standardFighterAbilities
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Fighter(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    skills: Skills,
    proficiencyBonus: ProficiencyBonus,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    fightingStyles: List[FighterFightingStyle] = List.empty,
    abilityUsages: BaseFighterAbilities = allUnused,
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List.empty[DamageType],
    damageImmunities: List[DamageType] = List.empty[DamageType],
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List.empty[ConditionType],
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    abilities: List[CombatantAbility] = standardFighterAbilities,
    conditions: List[Condition] = List.empty[Condition],
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName
) extends BaseFighter {

  val savingThrowProficiencies = NonEmptyList.of(Strength, Constitution)

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, offHand, fightingStyles)

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    applyDamage(this, adjustedDamage(dmg, damageType, this))

  def scoresCritical(roll: Int): Boolean = roll == 20

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Fighter {

  import BaseFighterAbilities._

  val standardFighterAbilities: List[CombatantAbility] = List(
    actionSurge(1),
    secondWind(2),
    extraAttack(3),
    twoWeaponFighting(4)
  )

  implicit def fighterShow[_: RS]: Show[Fighter] =
    Show.show { fighter =>
      s"Fighter: " +
        s"Name: ${fighter.name}, " +
        s"health: ${fighter.health}, " +
        s"AC: ${fighter.armourClass}"
    }

  // format: off
  val strengthLens: Lens[Fighter, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Fighter, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Fighter, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val intelligenceLens: Lens[Fighter, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val wisdomLens: Lens[Fighter, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val charismaLens: Lens[Fighter, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
  // format: on
}
