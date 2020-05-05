package io.github.tjheslin1.dmspredictor.classes.rogue

import cats.data.NonEmptyList
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogue.calculateArmourClass
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogueAbilities._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Rogue(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    skills: Skills,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    proficiencyBonus: ProficiencyBonus = 0,
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List.empty[DamageType],
    damageImmunities: List[DamageType] = List.empty[DamageType],
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List.empty[ConditionType],
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    abilities: List[CombatantAbility] = Rogue.standardRogueAbilities,
    hiddenFrom: List[Combatant] = List.empty[Combatant],
    conditions: List[Condition] = List.empty[Condition],
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName
) extends BaseRogue {

  val savingThrowProficiencies: NonEmptyList[Attribute] = NonEmptyList.of(Dexterity, Intelligence)

  def weapon[_: RS]: Weapon = baseWeapon

  val armourClass: Int = calculateArmourClass(stats, armour, offHand)

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    applyDamage(this, adjustedDamage(dmg, damageType, this))

  val reactionOnHit: Option[OnHitReaction] = None
  val reactionOnDamage: Option[OnDamageReaction] =
    if (level >= 5) uncannyDodge.some else none[OnDamageReaction]
}

object Rogue {

  val standardRogueAbilities: List[CombatantAbility] = List(
    hide(1),
    sneakAttack(2)
  )

  val strengthLens: Lens[Rogue, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Rogue, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Rogue, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val intelligenceLens: Lens[Rogue, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val wisdomLens: Lens[Rogue, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val charismaLens: Lens[Rogue, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
