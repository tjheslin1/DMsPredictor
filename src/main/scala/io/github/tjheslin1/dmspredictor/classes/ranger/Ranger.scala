package io.github.tjheslin1.dmspredictor.classes.ranger

import cats.Show
import cats.data.NonEmptyList
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRanger._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRangerAbilities._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}
import Ranger._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Concentration.handleConcentration
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.CureWounds
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._

@Lenses("_") case class Ranger(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    skills: Skills,
    spellSlots: SpellSlots,
    spellsKnown: Map[(SpellLevel, SpellEffect), Spell] = standardRangerSpellList,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    fightingStyles: List[RangerFightingStyle] = List.empty[RangerFightingStyle],
    proficiencyBonus: ProficiencyBonus = 2,
    resistances: List[DamageType] = List.empty,
    immunities: List[DamageType] = List.empty,
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    abilities: List[CombatantAbility] = standardRangerAbilities,
    conditions: List[Condition] = List.empty,
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    concentratingSpell: Option[spellcasting.Spell] = None,
    name: String = NameGenerator.randomName
) extends BaseRanger {

  val savingThrowProficiencies = NonEmptyList.of(Strength, Dexterity)

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = {
    val fightingStyleAppliedWeapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

    if (conditions.contains(HuntersMarkBuffCondition)) {
      lazy val extraDamage = huntersMarkDamage()

      Weapon.bonusDamageWeapon(fightingStyleAppliedWeapon, extraDamage)
    } else
      fightingStyleAppliedWeapon
  }

  def updateHealth[_: RS](
      dmg: Int,
      damageType: DamageType,
      attackResult: AttackResult
  ): Creature = {
    val damageTaken   = adjustedDamage(dmg, damageType, this)
    val updatedRanger = copy(health = Math.max(0, health - damageTaken))

    if (updatedRanger.isConscious == false && isConcentrating)
      handleConcentration(updatedRanger, updatedRanger.concentratingSpell.get, Integer.MAX_VALUE)
    else if (updatedRanger.isConscious && isConcentrating && damageTaken > 0)
      handleConcentration(updatedRanger, updatedRanger.concentratingSpell.get, damageTaken)
    else updatedRanger
  }

  def scoresCritical(roll: Int): Boolean = roll == 20

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Ranger {

  val standardRangerAbilities: List[CombatantAbility] = List(
    castSingleTargetHealingSpell(1),
    castSelfBuffSpell(2),
    extraAttack(3),
    twoWeaponFighting(4)
  )

  val standardRangerSpellList: Map[(SpellLevel, SpellEffect), Spell] = Map(
    (CureWounds.spellLevel, CureWounds.spellEffect)   -> CureWounds,
    (HuntersMark.spellLevel, HuntersMark.spellEffect) -> HuntersMark
  )

  implicit def rangerShow[_: RS]: Show[Ranger] = Show.show { ranger =>
    s"Ranger: " +
      s"Name: ${ranger.name}, " +
      s"health: ${ranger.health}, " +
      s"AC: ${ranger.armourClass}"
  }

  val strengthLens: Lens[Ranger, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Ranger, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Ranger, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Ranger, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Ranger, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Ranger, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)

  // format: off
  def rangerSpellSlots(level: Level): SpellSlots = level match {
    case LevelOne => SpellSlots(0, 0, 0)
    case LevelTwo => SpellSlots(2, 0, 0)
    case LevelThree => SpellSlots(3, 0, 0)
    case LevelFour => SpellSlots(3, 0, 0)
    case LevelFive => SpellSlots(4, 2, 0)
    case LevelTwenty => SpellSlots(4, 3, 3)
  }
  // format: on
}
