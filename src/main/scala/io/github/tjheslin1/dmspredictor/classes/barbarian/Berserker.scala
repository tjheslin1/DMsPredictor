package io.github.tjheslin1.dmspredictor.classes.barbarian

import cats.Show
import cats.data.NonEmptyList
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarian._
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarianAbilities._
import io.github.tjheslin1.dmspredictor.classes.barbarian.Berserker.standardBerserkerAbilities
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

@Lenses("_") case class Berserker(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    rageUsages: Int,
    skills: Skills,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    proficiencyBonus: ProficiencyBonus = 0,
    damageVulnerabilities: List[DamageType] = List.empty,
    damageResistances: List[DamageType] = List.empty,
    damageImmunities: List[DamageType] = List.empty,
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List.empty[ConditionType],
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    abilities: List[CombatantAbility] = standardBerserkerAbilities,
    conditions: List[Condition] = List.empty[Condition],
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    inRage: Boolean = false,
    inFrenzy: Boolean = false,
    rageTurnsLeft: Int = 10,
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName
) extends BaseBarbarian {

  val savingThrowProficiencies = NonEmptyList.of(Strength, Constitution)

  def weapon[_: RS]: Weapon = weaponWithRageDamage(baseWeapon, inRage || inFrenzy)

  val armourClass: Int = calculateArmourClass(stats, armour, offHand)

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    applyDamage(this, adjustedDamage(dmg, damageType, this))

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Berserker {

  import BerserkerAbilities._

  val standardBerserkerAbilities: List[CombatantAbility] = List(
    frenzy(1),
    extraAttack(2),
    bonusFrenzyAttack(3),
    recklessAttack(4)
  )

  implicit def berserkerShow[_: RS]: Show[Berserker] = Show.show { berserker =>
    s"Berserker: " +
      s"Name: ${berserker.name}, " +
      s"health: ${berserker.health}, " +
      s"AC: ${berserker.armourClass}"
  }

  val strengthLens: Lens[Berserker, Stat]  = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Berserker, Stat] = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Berserker, Stat] = _stats composeLens GenLens[BaseStats](
    _.constitution
  )
  val wisdomLens: Lens[Berserker, Stat] = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Berserker, Stat] = _stats composeLens GenLens[BaseStats](
    _.intelligence
  )
  val charismaLens: Lens[Berserker, Stat] = _stats composeLens GenLens[BaseStats](_.charisma)
}
