package io.github.tjheslin1.dmspredictor.classes.barbarian

import cats.Show
import cats.data.NonEmptyList
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian._
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarian._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.equipment.weapons.Greatsword
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Barbarian(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    rageUsages: Int,
    skills: Skills,
    proficiencyBonus: ProficiencyBonus,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List.empty[DamageType],
    damageImmunities: List[DamageType] = List.empty[DamageType],
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List.empty[ConditionType],
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    abilities: List[CombatantAbility] = standardBarbarianAbilities,
    conditions: List[Condition] = List.empty[Condition],
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    inRage: Boolean = false,
    rageTurnsLeft: Int = 10,
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName
) extends BaseBarbarian {

  val savingThrowProficiencies = NonEmptyList.of(Strength, Constitution)

  def weapon[_: RS]: Weapon = weaponWithRageDamage(baseWeapon, inRage)

  val armourClass: Int = calculateArmourClass(stats, armour, offHand)

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    applyDamage(this, adjustedDamage(dmg, damageType, this))

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Barbarian {

  import BaseBarbarianAbilities._

  val standardBarbarianAbilities: List[CombatantAbility] = List(
    rage(1),
    extraAttack(2),
    recklessAttack(3)
  )

  implicit def barbarianShow[_: RS]: Show[Barbarian] =
    Show.show { barbarian =>
      s"Barbarian: " +
        s"Name: ${barbarian.name}, " +
        s"health: ${barbarian.health}, " +
        s"AC: ${barbarian.armourClass}"
    }

  // format: off
  val strengthLens: Lens[Barbarian, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Barbarian, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Barbarian, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val intelligenceLens: Lens[Barbarian, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val wisdomLens: Lens[Barbarian, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val charismaLens: Lens[Barbarian, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
  // format: on
}
