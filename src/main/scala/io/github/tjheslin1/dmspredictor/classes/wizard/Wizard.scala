package io.github.tjheslin1.dmspredictor.classes.wizard

import cats.data.NonEmptyList
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.wizard.BaseWizard._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.macros.Lenses

@Lenses("_") case class Wizard(level: Level,
                               health: Int,
                               maxHealth: Int,
                               stats: BaseStats,
                               baseWeapon: Weapon,
                               skills: Skills,
                               cantripKnown: Option[Spell],
                               spellSlots: SpellSlots,
                               spellsKnown: Map[(SpellLevel, SpellEffect), Spell] = ???,
                               mageArmourPrepared: Boolean = true,
                               offHand: Option[Equipment] = None,
                               abilities: List[CombatantAbility] = ???,
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

  val armour: Armour = NoArmour

  val savingThrowProficiencies = NonEmptyList.of(Intelligence, Wisdom)

  def weapon[_: RS]: Weapon = baseWeapon

  val armourClass: Int = calculateArmourClass(stats, mageArmourPrepared)

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Wizard =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Wizard {

  val standardWizardSpellList: Map[(SpellLevel, SpellEffect), Spell] = Map(
    (FireBolt.spellLevel, FireBolt.spellEffect)         -> FireBolt,
    (MagicMissile.spellLevel, MagicMissile.spellEffect)         -> MagicMissile,

  )
}
