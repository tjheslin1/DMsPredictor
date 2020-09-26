package io.github.tjheslin1.dmspredictor.classes.cleric

import cats.data.NonEmptyList
import io.github.tjheslin1.dmspredictor.classes.cleric.BaseCleric._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric._
import io.github.tjheslin1.dmspredictor.classes.cleric.LifeClericAbilities._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastConditionSpell.castConditionSpell
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastSingleTargetHealingSpell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastSingleTargetOffensiveSpell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Concentration.handleConcentration
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Cleric(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    skills: Skills,
    proficiencyBonus: ProficiencyBonus,
    spellSlots: SpellSlots,
    spellsKnown: List[Spell] = standardClericSpellList,
    channelDivinityUsed: Boolean = false,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List.empty[DamageType],
    damageImmunities: List[DamageType] = List.empty[DamageType],
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List.empty[ConditionType],
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    abilities: List[model.CombatantAbility] = standardClericAbilities,
    conditions: List[Condition] = List.empty[Condition],
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    concentratingSpell: Option[Spell] = None,
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName
) extends BaseCleric {

  val savingThrowProficiencies = NonEmptyList.of(Wisdom, Charisma)

  def weapon[_: RS]: Weapon = baseWeapon

  val armourClass: Int = calculateArmourClass(stats, armour, offHand)

  def updateHealth[_: RS](
      dmg: Int,
      damageType: DamageType,
      attackResult: AttackResult
  ): Creature = {
    val damageTaken   = adjustedDamage(dmg, damageType, this)
    val updatedCleric = applyDamage(this, damageTaken).asInstanceOf[Cleric]

    if (updatedCleric.isConscious == false && isConcentrating)
      handleConcentration(updatedCleric, updatedCleric.concentratingSpell.get, Integer.MAX_VALUE)
    else if (updatedCleric.isConscious && isConcentrating && damageTaken > 0)
      handleConcentration(updatedCleric, updatedCleric.concentratingSpell.get, damageTaken)
    else
      updatedCleric
  }

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Cleric {

  import BaseClericAbilities._

  val standardClericSpellList: List[Spell] = List(
    SacredFlame,
    GuidingBolt,
    CureWounds,
    HoldPerson,
    SpiritGuardians
  )

  val standardClericAbilities: List[CombatantAbility] = List(
    preserveLife(1),
    discipleOfLife(2),
    castSingleTargetHealingSpell(3),
    destroyUndead(4),
    turnUndead(5),
    castConditionSpell(6),
    castSingleTargetOffensiveSpell(7)
  )

  val strengthLens: Lens[Cleric, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Cleric, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Cleric, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val intelligenceLens: Lens[Cleric, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val wisdomLens: Lens[Cleric, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val charismaLens: Lens[Cleric, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)

  // format: off
  def clericSpellSlots(level: Level): SpellSlots =
    level match {
      case LevelOne       => SpellSlots(2, 0, 0)
      case LevelTwo       => SpellSlots(3, 0, 0)
      case LevelThree     => SpellSlots(4, 2, 0)
      case LevelFour      => SpellSlots(4, 3, 0)
      case LevelFive      => SpellSlots(4, 3, 2)
      case LevelSix       => SpellSlots(4, 3, 3)
      case LevelSeven     => SpellSlots(4, 3, 3, 1, 0, 0, 0, 0, 0)
      case LevelEight     => SpellSlots(4, 3, 3, 2, 0, 0, 0, 0, 0)
      case LevelNine      => SpellSlots(4, 3, 3, 3, 1, 0, 0, 0, 0)
      case LevelTen       => SpellSlots(4, 3, 3, 3, 2, 0, 0, 0, 0)
      case LevelEleven    => SpellSlots(4, 3, 3, 3, 2, 1, 0, 0, 0)
      case LevelTwelve    => SpellSlots(4, 3, 3, 3, 2, 1, 0, 0, 0)
      case LevelThirteen  => SpellSlots(4, 3, 3, 3, 2, 1, 1, 0, 0)
      case LevelFourteen  => SpellSlots(4, 3, 3, 3, 2, 1, 1, 0, 0)
      case LevelFifteen   => SpellSlots(4, 3, 3, 3, 2, 1, 1, 1, 0)
      case LevelSixteen   => SpellSlots(4, 3, 3, 3, 2, 1, 1, 1, 0)
      case LevelSeventeen => SpellSlots(4, 3, 3, 3, 2, 1, 1, 1, 1)
      case LevelEighteen  => SpellSlots(4, 3, 3, 3, 3, 1, 1, 1, 1)
      case LevelNineteen  => SpellSlots(4, 3, 3, 3, 3, 2, 1, 1, 1)
      case LevelTwenty    => SpellSlots(4, 3, 3, 3, 3, 2, 2, 1, 1)
    }
    // format: on
}
