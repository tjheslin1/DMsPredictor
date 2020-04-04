package io.github.tjheslin1.dmspredictor.classes.ranger

import cats.Show
import cats.data.NonEmptyList
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRanger._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRangerAbilities.twoWeaponFighting
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Concentration.handleConcentration
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.CureWounds
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import Hunter._
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Hunter(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    skills: Skills,
    spellSlots: SpellSlots,
    spellsKnown: Map[(SpellLevel, SpellEffect), Spell] = standardHunterSpellList,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    fightingStyles: List[RangerFightingStyle] = List.empty[RangerFightingStyle],
    proficiencyBonus: ProficiencyBonus = 2,
    resistances: List[DamageType] = List.empty,
    immunities: List[DamageType] = List.empty,
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    colossusSlayerUsed: Boolean = false,
    abilities: List[CombatantAbility] = standardHunterAbilities,
    conditions: List[Condition] = List.empty,
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    concentratingSpell: Option[spellcasting.Spell] = None,
    name: String = NameGenerator.randomName
) extends BaseRanger {

  val savingThrowProficiencies = NonEmptyList.of(Strength, Dexterity)

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  def updateHealth[_: RS](
      dmg: Int,
      damageType: DamageType,
      attackResult: AttackResult
  ): Creature = {
    val damageTaken   = adjustedDamage(dmg, damageType, this)
    val updatedHunter = copy(health = Math.max(0, health - damageTaken))

    if (updatedHunter.isConscious == false && isConcentrating)
      handleConcentration(updatedHunter, updatedHunter.concentratingSpell.get, Integer.MAX_VALUE)
    else if (updatedHunter.isConscious && isConcentrating && damageTaken > 0)
      handleConcentration(updatedHunter, updatedHunter.concentratingSpell.get, damageTaken)
    else updatedHunter
  }

  def scoresCritical(roll: Int): Boolean = roll == 20

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Hunter {

  val standardHunterAbilities: List[CombatantAbility] = List(
    castSingleTargetHealingSpell(1),
    huntersMarkOnWeaponDamageAbility(2),
    castSelfBuffSpell(3),
    extraAttack(4),
    twoWeaponFighting(5)
  )

  val standardHunterSpellList: Map[(SpellLevel, SpellEffect), Spell] = Map(
    (CureWounds.spellLevel, CureWounds.spellEffect)   -> CureWounds,
    (HuntersMark.spellLevel, HuntersMark.spellEffect) -> HuntersMark
  )

  implicit def hunterShow[_: RS]: Show[Hunter] = Show.show { hunter =>
    s"Hunter: " +
      s"Name: ${hunter.name}, " +
      s"health: ${hunter.health}, " +
      s"AC: ${hunter.armourClass}"
  }

  val strengthLens: Lens[Hunter, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Hunter, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Hunter, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Hunter, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Hunter, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Hunter, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
