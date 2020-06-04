package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model.Modifier.attributeModifier
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich

import scala.annotation.tailrec

trait Spell {

  val name: String
  val school: SchoolOfMagic
  val castingTime: CastingTime
  val spellEffect: SpellEffect
  val spellLevel: SpellLevel
  val requiresConcentration: Boolean
  val benefitsFromHigherSpellSlot: Boolean

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant])
}

object Spell {

  /**
    *
    * @param checkCasterIsConcentrating is used to find the spell a caster had just used when finding the spell slot to update.
    *    It prevents looking further after finding a concentration spell because the spellCaster is now concentrating.
    */
  @tailrec
  def spellOfLevelOrBelow(
      spellCaster: SpellCaster,
      spellEffect: SpellEffect,
      spellLevel: SpellLevel
  )(
      originalSpellLevel: SpellLevel = spellLevel,
      checkCasterIsConcentrating: Boolean = true,
      singleTargetAttackOnly: Boolean = false,
      multiAttackOnly: Boolean = false
  ): Option[(Spell, SpellLevel)] = {
    val spellLookup = spellCaster.spellsKnown.get((spellLevel, spellEffect))

    val spellLevelBelow: SpellLevel = Refined.unsafeApply(spellLevel - 1)

    if (spellLookup.isDefined) {
      val spell = spellLookup.get

      // TODO test
      /*if (singleTargetAttackOnly && singleTargetAttackSpellOnly(spell) == false)
        spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(
          originalSpellLevel,
          checkCasterIsConcentrating,
          singleTargetAttackOnly,
          multiAttackOnly
        )
      else*/ if (multiAttackOnly && multiTargetAttackSpellOnly(spell) == false)
        spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(
          originalSpellLevel,
          checkCasterIsConcentrating,
          singleTargetAttackOnly,
          multiAttackOnly
        )
      else if (
        checkCasterIsConcentrating && spellCaster.isConcentrating && spell.requiresConcentration
      )
        spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(
          originalSpellLevel,
          checkCasterIsConcentrating,
          singleTargetAttackOnly,
          multiAttackOnly
        )
      else
        spellLookup match {
          case Some(foundSpell) if foundSpell.spellLevel.value == 0 =>
            (foundSpell, foundSpell.spellLevel).some
          case Some(foundSpell) if foundSpell.benefitsFromHigherSpellSlot =>
            (foundSpell, originalSpellLevel).some
          case Some(foundSpell) =>
            (foundSpell, foundSpell.spellLevel).some
          case _ => none[(Spell, SpellLevel)]
        }
    } else if (spellLevelBelow >= 0)
      spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(
        originalSpellLevel,
        checkCasterIsConcentrating,
        singleTargetAttackOnly,
        multiAttackOnly
      )
    else none[(Spell, SpellLevel)]
  }

  def singleTargetAttackSpellOnly(spell: Spell): Boolean =
    spell match {
      case _: SingleTargetInstantEffectSpell => true
      case _: SingleTargetSavingThrowSpell => true
      case _: SingleTargetAttackSpell => true
      case _: SingleTargetHealingSpell => true
      case _ => false
    }

  def multiTargetAttackSpellOnly(spell: Spell): Boolean =
    spell match {
      case _: MultiTargetSavingThrowSpell => true
      case _: MultiTargetBuffSpell => true
      case _ => false
    }

  def spellAttackBonus(spellCaster: SpellCaster): Int =
    attributeModifierForSchool(spellCaster) + spellCaster.spellCastingModifier

  def spellSaveDc(spellCaster: SpellCaster): Int =
    8 + attributeModifierForSchool(spellCaster) + spellCaster.spellCastingModifier

  def schoolAttribute(spellCaster: SpellCaster): Attribute =
    spellCaster match {
      case _: Cleric     => Wisdom
      case _: BaseCleric => Wisdom
      case _: Wizard     => Intelligence

      case _: Lich => Intelligence
    }

  def attributeModifierForSchool(spellCaster: SpellCaster): Int =
    attributeModifier(spellCaster, schoolAttribute(spellCaster))

  def spellSavingThrowPassed[_: RS](
      caster: SpellCaster,
      attribute: Attribute,
      target: Creature
  ): (Boolean, Creature) =
    savingThrowPassed(spellSaveDc(caster), attribute, target)

  def spellAttack[_: RS](spellCaster: SpellCaster, target: Creature): AttackResult =
    D20.roll() match {
      case roll if spellCaster.scoresCritical(roll) => CriticalHit
      case 1                                        => CriticalMiss
      case roll =>
        if ((roll + spellAttackBonus(spellCaster)) >= target.armourClass) Hit else Miss
    }
}
