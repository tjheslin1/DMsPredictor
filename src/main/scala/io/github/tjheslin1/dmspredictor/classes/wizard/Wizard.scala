package io.github.tjheslin1.dmspredictor.classes.wizard

import cats.data.NonEmptyList
import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.wizard.BaseWizard._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastMultiTargetOffensiveSpell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.CastSingleTargetOffensiveSpell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Concentration.handleConcentration
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Wizard(
    level: Level,
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    baseWeapon: Weapon,
    skills: Skills,
    proficiencyBonus: ProficiencyBonus,
    spellSlots: SpellSlots,
    spellsKnown: List[Spell] = Wizard.standardWizardSpellList,
    castShieldAsReaction: Boolean = true,
    mageArmourPrepared: Boolean = true,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = None,
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List.empty[DamageType],
    damageImmunities: List[DamageType] = List.empty[DamageType],
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List.empty[ConditionType],
    bonusActionUsed: Boolean = false,
    reactionUsed: Boolean = false,
    abilities: List[CombatantAbility] = Wizard.standardWizardAbilities,
    conditions: List[Condition] = List.empty[Condition],
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    concentratingSpell: Option[Spell] = None,
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName
) extends BaseWizard {

  val savingThrowProficiencies = NonEmptyList.of(Intelligence, Wisdom)

  def weapon[_: RS]: Weapon = baseWeapon

  val armourClass: Int = calculateArmourClass(stats, mageArmourPrepared, conditions)

  def updateHealth[_: RS](
      dmg: Int,
      damageType: DamageType,
      attackResult: AttackResult
  ): Creature = {
    val damageTaken   = adjustedDamage(dmg, damageType, this)
    val updatedWizard = applyDamage(this, damageTaken).asInstanceOf[Wizard]

    if (updatedWizard.isConscious == false && isConcentrating)
      handleConcentration(updatedWizard, updatedWizard.concentratingSpell.get, Integer.MAX_VALUE)
    else if (updatedWizard.isConscious && isConcentrating && damageTaken > 0)
      handleConcentration(updatedWizard, updatedWizard.concentratingSpell.get, damageTaken)
    else
      updatedWizard
  }

  val reactionOnHit: Option[OnHitReaction] =
    if (castShieldAsReaction)
      ShieldSpell.some
    else
      none[OnHitReaction]
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Wizard {

  val standardWizardSpellList: List[Spell] = List(
    FireBolt,
    MagicMissile,
    AcidArrow,
    Fireball
  )

  val standardWizardAbilities: List[CombatantAbility] = List(
    castMultiTargetOffensiveSpell(1),
    castSingleTargetOffensiveSpell(2)
  )

  val strengthLens: Lens[Wizard, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Wizard, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Wizard, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val intelligenceLens: Lens[Wizard, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val wisdomLens: Lens[Wizard, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val charismaLens: Lens[Wizard, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)

  def wizardSpellSlots(level: Level): SpellSlots =
    level match {
      case LevelOne =>
        SpellSlots(2, 0, 0)
      case LevelTwo =>
        SpellSlots(3, 0, 0)
      case LevelThree =>
        SpellSlots(4, 2, 0)
      case LevelFour =>
        SpellSlots(4, 3, 0)
      case LevelFive =>
        SpellSlots(4, 3, 2)
      case LevelSix =>
        SpellSlots(4, 3, 3)
      case LevelSeven =>
        SpellSlots(4, 3, 3, 1, 0, 0, 0, 0, 0)
      case LevelEight =>
        SpellSlots(4, 3, 3, 2, 0, 0, 0, 0, 0)
      case LevelNine =>
        SpellSlots(4, 3, 3, 3, 1, 0, 0, 0, 0)
      case LevelTen =>
        SpellSlots(4, 3, 3, 3, 2, 0, 0, 0, 0)
      case LevelEleven =>
        SpellSlots(4, 3, 3, 3, 2, 1, 0, 0, 0)
      case LevelTwelve =>
        SpellSlots(4, 3, 3, 3, 2, 1, 0, 0, 0)
      case LevelThirteen =>
        SpellSlots(4, 3, 3, 3, 2, 1, 1, 0, 0)
      case LevelFourteen =>
        SpellSlots(4, 3, 3, 3, 2, 1, 1, 0, 0)
      case LevelFifteen =>
        SpellSlots(4, 3, 3, 3, 2, 1, 1, 1, 0)
      case LevelSixteen =>
        SpellSlots(4, 3, 3, 3, 2, 1, 1, 1, 0)
      case LevelSeventeen =>
        SpellSlots(4, 3, 3, 3, 2, 1, 1, 1, 1)
      case LevelEighteen =>
        SpellSlots(4, 3, 3, 3, 3, 1, 1, 1, 1)
      case LevelNineteen =>
        SpellSlots(4, 3, 3, 3, 3, 2, 1, 1, 1)
      case LevelTwenty =>
        SpellSlots(4, 3, 3, 3, 3, 2, 2, 1, 1)
    }
}
