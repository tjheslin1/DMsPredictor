package io.github.tjheslin1.dmspredictor.classes.ranger

import cats.Show
import cats.data.NonEmptyList
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRanger._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRangerAbilities.twoWeaponFighting
import io.github.tjheslin1.dmspredictor.classes.ranger.Hunter._
import io.github.tjheslin1.dmspredictor.classes.ranger.HunterAbilities._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastSingleTargetHealingSpell.castSingleTargetHealingSpell
import io.github.tjheslin1.dmspredictor.model.spellcasting.Concentration.handleConcentration
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.CureWounds
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Hunter(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    skills: Skills,
    proficiencyBonus: ProficiencyBonus,
    spellSlots: SpellSlots,
    spellsKnown: List[Spell] = standardHunterSpellList,
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
    colossusSlayerUsed: Boolean = false,
    abilities: List[CombatantAbility] = standardHunterAbilities,
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
    val updatedHunter = applyDamage(this, damageTaken).asInstanceOf[Hunter]

    if (updatedHunter.isConscious == false && isConcentrating)
      handleConcentration(updatedHunter, updatedHunter.concentratingSpell.get, Integer.MAX_VALUE)
    else if (updatedHunter.isConscious && isConcentrating && damageTaken > 0)
      handleConcentration(updatedHunter, updatedHunter.concentratingSpell.get, damageTaken)
    else updatedHunter
  }

  def scoresCritical(roll: Int): Boolean = roll == 20

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None

  def resetStartOfTurn(): Creature = _colossusSlayerUsed.set(false)(this)
}

object Hunter {

  val standardHunterAbilities: List[CombatantAbility] = List(
    colossusSlayer(1),
    castSingleTargetHealingSpell(2),
    huntersMarkOnWeaponDamageAbility(3),
    castSelfBuffSpell(4),
    extraAttack(5),
    twoWeaponFighting(6)
  )

  val standardHunterSpellList: List[Spell] = List(
    CureWounds,
    HuntersMark
  )

  implicit def hunterShow[_: RS]: Show[Hunter] =
    Show.show { hunter =>
      s"Hunter: " +
        s"Name: ${hunter.name}, " +
        s"health: ${hunter.health}, " +
        s"AC: ${hunter.armourClass}"
    }

  // format: off
  val strengthLens: Lens[Hunter, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Hunter, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Hunter, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val intelligenceLens: Lens[Hunter, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val wisdomLens: Lens[Hunter, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val charismaLens: Lens[Hunter, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
  // format: on
}
