package io.github.tjheslin1.dmspredictor.classes.wizard

import cats.data.NonEmptyList
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.wizard.BaseWizard._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Wizard(level: Level,
                               health: Int,
                               maxHealth: Int,
                               stats: BaseStats,
                               baseWeapon: Weapon,
                               skills: Skills,
                               spellSlots: SpellSlots,
                               spellsKnown: Map[(SpellLevel, SpellEffect), Spell] =
                                 Wizard.standardWizardSpellList,
                               castShieldAsReaction: Boolean = true,
                               mageArmourPrepared: Boolean = true,
                               armour: Armour = NoArmour,
                               offHand: Option[Equipment] = None,
                               abilities: List[CombatantAbility] = Wizard.standardWizardAbilities,
                               conditions: List[Condition] = List.empty,
                               proficiencyBonus: ProficiencyBonus = 0,
                               resistances: List[DamageType] = List.empty,
                               immunities: List[DamageType] = List.empty,
                               bonusActionUsed: Boolean = false,
                               reactionUsed: Boolean = false,
                               attackStatus: AttackStatus = Regular,
                               defenseStatus: AttackStatus = Regular,
                               concentratingSpell: Option[Spell] = None,
                               name: String = NameGenerator.randomName)
    extends BaseWizard {

  val savingThrowProficiencies = NonEmptyList.of(Intelligence, Wisdom)

  def weapon[_: RS]: Weapon = baseWeapon

  val armourClass: Int = calculateArmourClass(stats, mageArmourPrepared, conditions)

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Wizard =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))

  val reactionOnHit: Option[OnHitReaction] =
    if (castShieldAsReaction) ShieldSpell.some else none[OnHitReaction]
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Wizard {

  val standardWizardSpellList: Map[(SpellLevel, SpellEffect), Spell] = Map(
    (FireBolt.spellLevel, FireBolt.spellEffect)         -> FireBolt,
    (MagicMissile.spellLevel, MagicMissile.spellEffect) -> MagicMissile,
    (AcidArrow.spellLevel, AcidArrow.spellEffect)       -> AcidArrow,
    (Fireball.spellLevel, Fireball.spellEffect)         -> Fireball
  )

  val standardWizardAbilities: List[CombatantAbility] = List(
    castMultiTargetOffensiveSpell(1),
    castSingleTargetOffensiveSpell(2)
  )

  val strengthLens: Lens[Wizard, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Wizard, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Wizard, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Wizard, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Wizard, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Wizard, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)

  // format: off
  def wizardSpellSlots(level: Level): SpellSlots = level match {
    case LevelOne => SpellSlots(2, 0, 0)
    case LevelTwo => SpellSlots(3, 0, 0)
    case LevelThree => SpellSlots(4, 2, 0)
    case LevelFour => SpellSlots(4, 3, 0)
    case LevelFive => SpellSlots(4, 3, 2)
    case LevelTwenty => SpellSlots(4, 3, 3)
  }
  // format: on
}
