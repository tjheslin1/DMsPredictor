package util

import cats.syntax.option.none
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.HandleDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, ConditionType}
import io.github.tjheslin1.dmspredictor.model.spellcasting.{Spell, SpellLevel, SpellSlots}
import io.github.tjheslin1.dmspredictor.monsters.Monster
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}
import util.TestMonster.defaultScores

@Lenses("_") case class TestSpellCastingMonster(
    health: Int,
    maxHealth: Int,
    stats: BaseStats,
    armourClass: Int,
    baseWeapon: Weapon,
    armour: Armour = NoArmour,
    offHand: Option[Equipment] = none[Equipment],
    damageVulnerabilities: List[DamageType] = List.empty[DamageType],
    damageResistances: List[DamageType] = List.empty[DamageType],
    damageImmunities: List[DamageType] = List.empty[DamageType],
    conditionResistances: List[ConditionType] = List.empty[ConditionType],
    conditionImmunities: List[ConditionType] = List.empty[ConditionType],
    abilities: List[CombatantAbility] = List.empty[CombatantAbility],
    conditions: List[Condition] = List.empty[Condition],
    reactionUsed: Boolean = false,
    attackStatus: AttackStatus = Regular,
    defenseStatus: AttackStatus = Regular,
    creatureType: CreatureType = Humanoid,
    challengeRating: Double = 1,
    perceptionScore: Int = 0,
    stealthScore: Int = 0,
    savingThrowScores: Map[Attribute, Int] = defaultScores,
    spellsKnown: Map[(SpellLevel, spellcasting.SpellEffect), Spell] = Map.empty,
    spellSlots: SpellSlots = SpellSlots(0, 0, 0),
    concentratingSpell: Option[Spell] = none[Spell],
    spellCastingModifier: Int = 0,
    spellCastingLevel: Level = LevelOne,
    isAlive: Boolean = true,
    name: String = NameGenerator.randomName)
    extends Monster
    with SpellCaster {

  val levelSpellcastingLearned: Level = LevelOne

  def weapon[_: RS]: Weapon = weapon

  val skills: Skills = Skills(1, 1)

  def scoresCritical(roll: Int): Boolean = roll == 20

  def resetStartOfTurn(): Creature = this

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))
}

object TestSpellCastingMonster {

  val defaultScores = Map(
    Strength     -> 0,
    Dexterity    -> 0,
    Constitution -> 0,
    Wisdom       -> 0,
    Intelligence -> 0,
    Charisma     -> 0
  )

  // format: off
  val strengthLens: Lens[TestSpellCastingMonster, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[TestSpellCastingMonster, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[TestSpellCastingMonster, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[TestSpellCastingMonster, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[TestSpellCastingMonster, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[TestSpellCastingMonster, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
  // format: on
}
