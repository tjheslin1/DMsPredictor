package io.github.tjheslin1.dmspredictor.classes.ranger

import cats.Show
import cats.data.NonEmptyList
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRanger._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRangerAbilities._
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastSelfBuffSpell.castSelfBuffSpell
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastSingleTargetHealingSpell.castSingleTargetHealingSpell
import io.github.tjheslin1.dmspredictor.model.spellcasting.Concentration.handleConcentration
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.CureWounds
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Ranger(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    skills: Skills,
    proficiencyBonus: ProficiencyBonus,
    spellSlots: SpellSlots,
    spellsKnown: List[Spell] = standardRangerSpellList,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    fightingStyles: List[RangerFightingStyle] = List.empty[RangerFightingStyle],
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List.empty[DamageType],
    damageImmunities: List[DamageType] = List.empty[DamageType],
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List.empty[ConditionType],
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    abilities: List[CombatantAbility] = standardRangerAbilities,
    conditions: List[Condition] = List.empty[Condition],
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    concentratingSpell: Option[spellcasting.Spell] = None,
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName
) extends BaseRanger {

  val savingThrowProficiencies = NonEmptyList.of(Strength, Dexterity)

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, offHand, fightingStyles)

  def updateHealth[_: RS](
      dmg: Int,
      damageType: DamageType,
      attackResult: AttackResult
  ): Creature = {
    val damageTaken   = adjustedDamage(dmg, damageType, this)
    val updatedRanger = applyDamage(this, damageTaken).asInstanceOf[Ranger]

    if (updatedRanger.isConscious == false && isConcentrating)
      handleConcentration(updatedRanger, updatedRanger.concentratingSpell.get, Integer.MAX_VALUE)
    else if (updatedRanger.isConscious && isConcentrating && damageTaken > 0)
      handleConcentration(updatedRanger, updatedRanger.concentratingSpell.get, damageTaken)
    else updatedRanger
  }

  def scoresCritical(roll: Int): Boolean = roll == 20

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None

  def resetStartOfTurn(): Creature = this
}

object Ranger {

  val standardRangerAbilities: List[CombatantAbility] = List(
    castSingleTargetHealingSpell(1),
    huntersMarkOnWeaponDamageAbility(2),
    castSelfBuffSpell(3),
    extraAttack(4),
    twoWeaponFighting(5)
  )

  val standardRangerSpellList: List[Spell] = List(
    CureWounds,
    HuntersMark
  )

  implicit def rangerShow[_: RS]: Show[Ranger] =
    Show.show { ranger =>
      s"Ranger: " +
        s"Name: ${ranger.name}, " +
        s"health: ${ranger.health}, " +
        s"AC: ${ranger.armourClass}"
    }

  // format: off
  val strengthLens: Lens[Ranger, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Ranger, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Ranger, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val intelligenceLens: Lens[Ranger, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val wisdomLens: Lens[Ranger, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val charismaLens: Lens[Ranger, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
  // format: on
}
