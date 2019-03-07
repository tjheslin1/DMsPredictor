package io.github.tjheslin1.dmspredictor.classes.cleric

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.cleric.BaseCleric._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric._
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.{CureWounds, GuidingBolt, SacredFlame}
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Cleric(level: Level,
                               health: Int,
                               maxHealth: Int,
                               stats: BaseStats,
                               baseWeapon: Weapon,
                               cantripKnown: Option[Spell],
                               spellSlots: SpellSlots,
                               spellsKnown: Map[(SpellLevel, SpellEffect), Spell] = standardClericSpellList,
                               channelDivinityUsed: Boolean = false,
                               armour: Armour = NoArmour,
                               offHand: Option[Equipment] = None,
                               abilities: List[CombatantAbility] = standardClericAbilities,
                               conditions: List[Condition] = List.empty,
                               proficiencyBonus: ProficiencyBonus = 0,
                               resistances: List[DamageType] = List.empty,
                               immunities: List[DamageType] = List.empty,
                               bonusActionUsed: Boolean = false,
                               attackStatus: AttackStatus = Regular,
                               defenseStatus: model.AttackStatus = Regular,
                               name: String = NameGenerator.randomName)
    extends BaseCleric {

  def weapon[_: RS]: Weapon = baseWeapon

  val armourClass: Int = calculateArmourClass(stats, armour, offHand)

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))

  def scoresCritical(roll: Int): Boolean = roll == 20
}

object Cleric {

  import BaseClericAbilities._

  val standardClericSpellList: Map[(SpellLevel, SpellEffect), Spell] = Map(
    (SacredFlame.spellLevel, SacredFlame.spellEffect) -> SacredFlame,
    (GuidingBolt.spellLevel, GuidingBolt.spellEffect) -> GuidingBolt,
    (CureWounds.spellLevel, CureWounds.spellEffect) -> GuidingBolt
  )

  val standardClericAbilities: List[CombatantAbility] = List(
    castSingleTargetHealingSpell(1),
    destroyUndead(2),
    turnUndead(3),
    castSingleTargetOffensiveSpell(4)
  )

  val strengthLens: Lens[Cleric, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Cleric, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Cleric, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Cleric, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Cleric, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Cleric, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
