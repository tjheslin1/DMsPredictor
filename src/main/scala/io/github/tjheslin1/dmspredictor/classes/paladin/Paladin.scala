package io.github.tjheslin1.dmspredictor.classes.paladin

import cats.Show
import cats.data.NonEmptyList
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.paladin.BasePaladin._
import io.github.tjheslin1.dmspredictor.classes.paladin.BasePaladinAbilities._
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Concentration.handleConcentration
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.CureWounds
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Paladin(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    skills: Skills,
    spellSlots: SpellSlots,
    spellsKnown: Map[(SpellLevel, SpellEffect), Spell] = Map.empty, // TODO
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    fightingStyles: List[PaladinFightingStyle] = List.empty[PaladinFightingStyle],
    proficiencyBonus: ProficiencyBonus = 2,
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List.empty[DamageType],
    damageImmunities: List[DamageType] = List.empty[DamageType],
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List.empty[ConditionType],
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    abilities: List[CombatantAbility] = List.empty, // TODO
    conditions: List[Condition] = List.empty[Condition],
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    concentratingSpell: Option[spellcasting.Spell] = None,
    isAlive: Boolean = true,
    layOnHandsPool: Int,
    name: String = NameGenerator.randomName
) extends BasePaladin {

  val savingThrowProficiencies = NonEmptyList.of(Wisdom, Charisma)

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, offHand, fightingStyles)

  def updateHealth[_: RS](
      dmg: Int,
      damageType: DamageType,
      attackResult: AttackResult
  ): Creature = {
    val damageTaken    = adjustedDamage(dmg, damageType, this)
    val updatedPaladin = applyDamage(this, damageTaken).asInstanceOf[Paladin]

    if (updatedPaladin.isConscious == false && isConcentrating)
      handleConcentration(updatedPaladin, updatedPaladin.concentratingSpell.get, Integer.MAX_VALUE)
    else if (updatedPaladin.isConscious && isConcentrating && damageTaken > 0)
      handleConcentration(updatedPaladin, updatedPaladin.concentratingSpell.get, damageTaken)
    else updatedPaladin
  }

  def scoresCritical(roll: Int): Boolean = roll == 20

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Paladin {

  val standardPaladinSpellList: Map[(SpellLevel, SpellEffect), Spell] = Map(
    (CureWounds.spellLevel, CureWounds.spellEffect) -> CureWounds
  )

  val standardPaladinAbilities: List[CombatantAbility] = List(
    castSingleTargetHealingSpell(1),
    layOnHands(2),
    extraAttack(3),
    divineSmite(4)
  )

  implicit def paladinShow[_: RS]: Show[Ranger] =
    Show.show { ranger =>
      s"Paladin: " +
        s"Name: ${ranger.name}, " +
        s"health: ${ranger.health}, " +
        s"AC: ${ranger.armourClass}"
    }

  // format: off
  val strengthLens: Lens[Paladin, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Paladin, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Paladin, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val intelligenceLens: Lens[Paladin, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val wisdomLens: Lens[Paladin, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val charismaLens: Lens[Paladin, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
  // format: on
}
