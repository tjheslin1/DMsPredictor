package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Paralyzed, StartOfTurnCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object ClericSpells extends LazyLogging {

  case object SacredFlame extends SingleTargetSavingThrowSpell {
    val savingThrowAttribute: Attribute = Dexterity
    val halfDamageOnSave: Boolean       = false

    val damageType: DamageType   = Radiant
    val name                     = "Sacred Flame"
    val school: SchoolOfMagic    = Evocation
    val castingTime: CastingTime = OneActionCast
    val spellLevel: SpellLevel   = 0
    val requiresConcentration    = false
    val useHigherSpellSlot       = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int =
      spellCaster.spellCastingLevel.value match {
        case lvl if lvl >= 17 => 4 * D8
        case lvl if lvl >= 11 => 3 * D8
        case lvl if lvl >= 5  => 2 * D8
        case _                => 1 * D8
      }
  }

  case object GuidingBolt extends SingleTargetAttackSpell {
    val damageType: DamageType             = Radiant
    val name                               = "Guiding Bolt"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneActionCast
    val spellTargetStyle: SpellTargetStyle = RangedSpellAttack
    val spellLevel: SpellLevel             = 1
    val requiresConcentration: Boolean     = false
    val useHigherSpellSlot                 = true
    val halfDamageOnMiss                   = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = (3 + spellLevel) * D6

    // TODO Apply advantage effect
    override def additionalEffect(target: Combatant, attackResult: AttackResult): Combatant =
      target
  }

  case object CureWounds extends SingleTargetHealingSpell {
    val name                               = "Cure Wounds"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneActionCast
    val spellTargetStyle: SpellTargetStyle = MeleeSpellAttack
    val spellLevel: SpellLevel             = 1
    val requiresConcentration: Boolean     = false
    val useHigherSpellSlot                 = true

    def healing[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int =
      (spellLevel.value * D8) + attributeModifierForSchool(spellCaster)
  }

  case object HoldPerson extends ConcentrationConditionSpell {
    val name: String = "Hold Person"

    val singleTarget: Boolean = true
    val attribute: Attribute  = Wisdom

    val school: SchoolOfMagic    = Enchantment
    val castingTime: CastingTime = OneActionCast
    val spellLevel: SpellLevel   = 2
    val useHigherSpellSlot       = false

    def conditionFrom(spellCaster: SpellCaster): Condition =
      Paralyzed(spellSaveDc(spellCaster), 10, attribute)
  }

  case object SpiritGuardians extends ConcentrationConditionSpell {
    val name = "Spirit Guardians"

    val singleTarget: Boolean = false
    val attribute: Attribute  = Wisdom

    val school: SchoolOfMagic    = Conjuration
    val castingTime: CastingTime = OneActionCast
    val spellLevel: SpellLevel   = 3
    val useHigherSpellSlot       = false

    def conditionFrom(spellCaster: SpellCaster): Condition =
      SpiritGuardiansCondition(spellLevel, spellSaveDc(spellCaster), 100, Wisdom)
  }

  case class SpiritGuardiansCondition(
      spellLevel: SpellLevel,
      saveDc: Int,
      turnsLeft: Int,
      attribute: Attribute,
      name: String = "Spirit Guardians (attack)"
  ) extends StartOfTurnCondition {
    val missesTurn: Boolean        = false
    val isHandledOnDamage: Boolean = false

    def decrementTurnsLeft(): Condition = this.copy(turnsLeft = this.turnsLeft - 1)

    def handleStartOfTurn[_: RS](creature: Creature): Creature = {
      val damage = spellLevel.value * D8

      logger.debug(s"${creature.name} takes damage from ${SpiritGuardians.name}")

      if (savingThrowPassed(saveDc, Wisdom, creature))
        creature.updateHealth(Math.floor(damage / 2).toInt, Radiant, Hit)
      else
        creature.updateHealth(damage, Radiant, Hit)
    }
  }
}
