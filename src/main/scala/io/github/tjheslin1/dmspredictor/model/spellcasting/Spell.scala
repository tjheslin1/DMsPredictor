package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
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
  val useHigherSpellSlot: Boolean

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant])
}

object Spell {

  /**
    *
    * @param checkConcentration is used to find the spell a caster had just used when finding the spell slot to update
    */
  @tailrec
  def spellOfLevelOrBelow(
      spellCaster: SpellCaster,
      spellEffect: SpellEffect,
      spellLevel: SpellLevel
  )(
      originalSpellLevel: SpellLevel = spellLevel,
      checkConcentration: Boolean = true,
      multiAttackOnly: Boolean = false
  ): Option[(Spell, SpellLevel)] = {
    val spellLookup = spellCaster.spellsKnown.get((spellLevel, spellEffect))

    val spellLevelBelow: SpellLevel = Refined.unsafeApply(spellLevel - 1)

    if (spellLookup.isDefined) {
      val spell = spellLookup.get

      if (multiAttackOnly && spell.isInstanceOf[MultiTargetSavingThrowSpell] == false)
        spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(originalSpellLevel)
      else if (checkConcentration && spellCaster.isConcentrating && spell.requiresConcentration)
        spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(originalSpellLevel)
      else
        spellLookup match {
          case Some(foundSpell) if foundSpell.spellLevel.value == 0 =>
            (foundSpell, foundSpell.spellLevel).some
          case Some(foundSpell) if foundSpell.useHigherSpellSlot =>
            (foundSpell, originalSpellLevel).some
          case Some(foundSpell) => (foundSpell, foundSpell.spellLevel).some
          case _                => none[(Spell, SpellLevel)]
        }
    } else if (spellLevelBelow >= 0)
      spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)(originalSpellLevel)
    else none[(Spell, SpellLevel)]
  }

  def spellAttackBonus(spellCaster: SpellCaster): Int =
    attributeModifierForSchool(spellCaster) + spellCaster.spellCastingModifier

  def spellSaveDc(spellCaster: SpellCaster): Int =
    8 + attributeModifierForSchool(spellCaster) + spellCaster.spellCastingModifier

  def schoolAttribute(spellcaster: SpellCaster): Attribute = spellcaster match {
    case _: Cleric     => Wisdom
    case _: BaseCleric => Wisdom
    case _: Wizard     => Intelligence

    case _: Lich => Intelligence
  }

  def attributeModifierForSchool(spellcaster: SpellCaster): Int =
    attributeModifier(spellcaster, schoolAttribute(spellcaster))

  def spellSavingThrowPassed[_: RS](
      caster: SpellCaster,
      attribute: Attribute,
      target: Creature
  ): Boolean =
    savingThrowPassed(spellSaveDc(caster), attribute, target)

  def spellAttack[_: RS](spellCaster: SpellCaster, target: Creature): AttackResult =
    D20.roll() match {
      case roll if spellCaster.scoresCritical(roll) => CriticalHit
      case 1                                        => CriticalMiss
      case roll =>
        if ((roll + spellAttackBonus(spellCaster)) >= target.armourClass) Hit else Miss
    }
}
