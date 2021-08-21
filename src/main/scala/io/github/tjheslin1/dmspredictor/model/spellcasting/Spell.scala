package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model.Modifier.attributeModifier
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.PaladinSpells.blessAttackBonus
import io.github.tjheslin1.dmspredictor.util.IntOps._

import scala.annotation.tailrec

trait Spell {

  val name: String
  val school: SchoolOfMagic
  val castingTime: CastingTime
  val spellEffect: SpellEffect
  val spellLevel: SpellLevel
  val requiresConcentration: Boolean
  val benefitsFromHigherSpellSlot: Boolean
  val isSingleTargetSpell: Boolean
  val isMultiTargetSpell: Boolean

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant])
}

object Spell {

  /** @param findNewlyConcentratingSpell
    *   is used to find the spell a caster had just used when finding the spell slot to update. It
    *   prevents looking further after finding a concentration spell because the spellCaster is now
    *   concentrating.
    */
  @tailrec
  def spellOfLevelOrBelow(
      spellCaster: SpellCaster,
      spellEffect: SpellEffect,
      spellLevel: SpellLevel
  )(
      originalSpellLevel: SpellLevel = spellLevel,
      findNewlyConcentratingSpell: Boolean = true,
      singleTargetSpellsOnly: Boolean = false,
      multiTargetSpellsOnly: Boolean = false
  ): Option[(Spell, SpellLevel)] = {
    def foundSpellMatches(foundSpell: Spell): Boolean = {
      val spellTypeMatches =
        if (singleTargetSpellsOnly && foundSpell.isSingleTargetSpell == false)
          false
        else if (multiTargetSpellsOnly && foundSpell.isMultiTargetSpell == false)
          false
        else
          true

      spellTypeMatches &&
      foundSpell.spellLevel == spellLevel &&
      foundSpell.spellEffect == spellEffect
    }

    val spellLookup = spellCaster.spellsKnown.find {
      case foundSpell if foundSpellMatches(foundSpell) =>
        true
      case _ =>
        false
    }

    val spellLevelBelow: SpellLevel = Refined.unsafeApply(spellLevel - 1)

    if (spellLookup.isDefined) {
      val spell = spellLookup.get

      if (singleTargetSpellsOnly && spell.isSingleTargetSpell == false)
        spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(
          originalSpellLevel,
          findNewlyConcentratingSpell,
          singleTargetSpellsOnly,
          multiTargetSpellsOnly
        )
      else if (multiTargetSpellsOnly && spell.isMultiTargetSpell == false)
        spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(
          originalSpellLevel,
          findNewlyConcentratingSpell,
          singleTargetSpellsOnly,
          multiTargetSpellsOnly
        )
      else if (
        findNewlyConcentratingSpell == false && spellCaster.isConcentrating && spell.requiresConcentration
      )
        spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(
          originalSpellLevel,
          findNewlyConcentratingSpell,
          singleTargetSpellsOnly,
          multiTargetSpellsOnly
        )
      else
        spellLookup match {
          case Some(foundSpell) if foundSpell.spellLevel.value == 0 =>
            (foundSpell, foundSpell.spellLevel).some
          case Some(foundSpell) if foundSpell.benefitsFromHigherSpellSlot =>
            (foundSpell, originalSpellLevel).some
          case Some(foundSpell) =>
            (foundSpell, foundSpell.spellLevel).some
          case _ =>
            none[(Spell, SpellLevel)]
        }
    } else if (spellLevelBelow >= 0)
      spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(
        originalSpellLevel,
        findNewlyConcentratingSpell,
        singleTargetSpellsOnly,
        multiTargetSpellsOnly
      )
    else
      none[(Spell, SpellLevel)]
  }

  def spellAttackBonus(spellCaster: SpellCaster): Int =
    attributeModifierForSchool(spellCaster) + spellCaster.spellCastingModifier

  def spellSaveDc(spellCaster: SpellCaster): Int =
    8 + attributeModifierForSchool(spellCaster) + spellCaster.spellCastingModifier

  def attributeModifierForSchool(spellCaster: SpellCaster): Int = attributeModifier(
    spellCaster,
    spellCaster.spellCastingAttribute)

  def spellSavingThrowPassed[_: RS](
      caster: SpellCaster,
      attribute: Attribute,
      target: Creature
  ): (Boolean, Creature) = savingThrowPassed(spellSaveDc(caster), attribute, target)

  def spellAttack[_: RS](spellCaster: SpellCaster, target: Creature): AttackResult =
    D20.roll() match {
      case roll if spellCaster.scoresCritical(roll) =>
        CriticalHit
      case 1 =>
        CriticalMiss
      case roll =>
        val attackRoll = roll + spellAttackBonus(spellCaster) + blessAttackBonus(spellCaster)

        if (attackRoll >= target.armourClass)
          Hit
        else
          Miss
    }
}
