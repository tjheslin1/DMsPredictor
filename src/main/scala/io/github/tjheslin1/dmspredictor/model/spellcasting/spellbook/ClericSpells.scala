package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition.{addCondition, removeCondition}
import io.github.tjheslin1.dmspredictor.model.condition._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object ClericSpells extends LazyLogging {

  case object SacredFlame extends SingleTargetSavingThrowSpell {
    val savingThrowAttribute: Attribute = Dexterity
    val halfDamageOnSave: Boolean       = false

    val damageType: DamageType      = Radiant
    val name                        = "Sacred Flame"
    val school: SchoolOfMagic       = Evocation
    val castingTime: CastingTime    = OneActionCast
    val spellLevel: SpellLevel      = 0
    val requiresConcentration       = false
    val benefitsFromHigherSpellSlot = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int =
      spellCaster.spellCastingLevel.value match {
        case lvl if lvl >= 17 => 4 * D8
        case lvl if lvl >= 11 => 3 * D8
        case lvl if lvl >= 5  => 2 * D8
        case _                => 1 * D8
      }
  }

  case object GuidingBolt extends SingleTargetAttackSpell {
    val name                        = "Guiding Bolt"
    val damageType                  = Radiant
    val school                      = Evocation
    val castingTime                 = OneActionCast
    val spellTargetStyle            = RangedSpellAttack
    val spellLevel                  = 1
    val benefitsFromHigherSpellSlot = true
    val halfDamageOnMiss            = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int =
      (3 + spellLevel) * D6

    override def additionalEffect[_: RS](target: Combatant, attackResult: AttackResult): Combatant =
      attackResult match {
        case CriticalHit | Hit =>
          addCondition(target, GuidingBoltCondition())
        case CriticalMiss | Miss => target
      }
  }

  case class GuidingBoltCondition(turnsLeft: Int = 2) extends OnDamageCondition {
    val name              = "Guiding Bolt (Condition)"
    val missesTurn        = false
    val saveDc            = 0
    val isHandledOnDamage = true

    def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature =
      removeCondition(creature, name)

    def decrementTurnsLeft(): Condition =
      GuidingBoltCondition(turnsLeft - 1)

    def onConditionApplied[_: RS](creature: Creature): Creature =
      Creature.creatureDefenseStatusLens.set(Disadvantage)(creature)

    def onConditionRemoved[_: RS](creature: Creature): Creature =
      Creature.creatureDefenseStatusLens.set(Regular)(creature)
  }

  case object CureWounds extends SingleTargetHealingSpell {
    val name                               = "Cure Wounds"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneActionCast
    val spellTargetStyle: SpellTargetStyle = MeleeSpellAttack
    val spellLevel: SpellLevel             = 1
    val benefitsFromHigherSpellSlot        = true

    def healing[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int =
      (spellLevel.value * D8) + attributeModifierForSchool(spellCaster)
  }

  case object HoldPerson extends ConditionSpell {
    val name = "Hold Person"

    val attribute = Wisdom

    val affectedTargets       = 1
    val requiresConcentration = true

    val conditionTargetsPriority: Ordering[Combatant] = (x: Combatant, y: Combatant) =>
      if (x.creature.isConscious) 1
      else if (y.creature.isConscious) -1
      else x.creature.health.compare(y.creature.health)

    val school: SchoolOfMagic       = Enchantment
    val castingTime: CastingTime    = OneActionCast
    val spellLevel: SpellLevel      = 2
    val benefitsFromHigherSpellSlot = false

    def conditionFrom(spellCaster: SpellCaster): Condition =
      Paralyzed(spellSaveDc(spellCaster), 10, attribute)
  }

  case object SpiritGuardians extends ConditionSpell {
    val name = "Spirit Guardians"

    val attribute = Wisdom

    val affectedTargets       = Int.MaxValue
    val requiresConcentration = true

    val conditionTargetsPriority: Ordering[Combatant] = (x: Combatant, y: Combatant) =>
      if (x.creature.isConscious) 1
      else if (y.creature.isConscious) -1
      else x.creature.health.compare(y.creature.health)

    val school: SchoolOfMagic    = Conjuration
    val castingTime: CastingTime = OneActionCast
    val spellLevel: SpellLevel   = 3

    val benefitsFromHigherSpellSlot = false

    def conditionFrom(spellCaster: SpellCaster): Condition =
      SpiritGuardiansCondition(spellLevel, spellSaveDc(spellCaster), 100, Wisdom)
  }

  case class SpiritGuardiansCondition(
      spellLevel: SpellLevel,
      saveDc: Int,
      turnsLeft: Int,
      attribute: Attribute
  ) extends StartOfTurnCondition {
    val name              = "Spirit Guardians (attack)"
    val missesTurn        = false
    val isHandledOnDamage = false

    def decrementTurnsLeft(): Condition =
      this.copy(turnsLeft = this.turnsLeft - 1)

    def handleStartOfTurn[_: RS](creature: Creature): Creature = {
      val damage = spellLevel.value * D8

      logger.debug(s"${creature.name} takes damage from ${SpiritGuardians.name}")

      val (passed, updatedCreature) =
        savingThrowPassed(saveDc, Wisdom, creature)

      if (passed)
        updatedCreature.updateHealth(Math.floor(damage / 2).toInt, Radiant, Hit)
      else
        updatedCreature.updateHealth(damage, Radiant, Hit)
    }

    def onConditionApplied[_: RS](creature: Creature): Creature = creature
    def onConditionRemoved[_: RS](creature: Creature): Creature = creature
  }
}
