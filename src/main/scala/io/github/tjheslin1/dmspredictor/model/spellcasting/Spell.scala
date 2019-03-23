package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model.Modifier.attributeModifier
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._

import scala.annotation.tailrec

abstract class Spell {

  val name: String
  val school: SchoolOfMagic
  val castingTime: CastingTime
  val spellEffect: SpellEffect
  val spellLevel: SpellLevel
  val requiresConcentration: Boolean

  def effect[_: RS](spellCaster: SpellCaster,
                    spellLevel: SpellLevel,
                    targets: List[Combatant]): (SpellCaster, List[Combatant])
}

object Spell {

  @tailrec
  def spellOfLevelOrBelow(spellCaster: SpellCaster,
                          spellEffect: SpellEffect,
                          spellLevel: SpellLevel): Option[Spell] = {
    val spellLookup = spellCaster.spellsKnown.get((spellLevel, spellEffect))

    val spellLevelBelow: SpellLevel = Refined.unsafeApply(spellLevel - 1)

    if (spellLookup.isDefined) {
      if (spellCaster.isConcentrating && spellLookup.get.requiresConcentration)
        spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)
      else
        spellLookup
    } else if (spellLevelBelow >= 0) spellOfLevelOrBelow(spellCaster, spellEffect, spellLevelBelow)
    else none[Spell]
  }

  def spellAttackBonus(spellCaster: SpellCaster): Int = spellCaster match {
    case playerSpellcaster: Player with SpellCaster =>
      playerSpellcaster.proficiencyBonus + attributeModifierForSchool(playerSpellcaster)
    case spellcaster => attributeModifierForSchool(spellcaster)
  }

  def spellSaveDc(spellCaster: SpellCaster): Int = spellCaster match {
    case playerSpellcaster: Player with SpellCaster =>
      8 + playerSpellcaster.proficiencyBonus + attributeModifierForSchool(playerSpellcaster)
    case spellcaster => 8 + attributeModifierForSchool(spellcaster)
  }

  def schoolAttribute(spellcaster: SpellCaster): Attribute = spellcaster match {
    case _: Cleric     => Wisdom
    case _: BaseCleric => Wisdom
  }

  def attributeModifierForSchool(spellcaster: SpellCaster): Int =
    attributeModifier(spellcaster, schoolAttribute(spellcaster))

  def spellSavingThrowPassed[_: RS](caster: SpellCaster,
                                    attribute: Attribute,
                                    target: Creature): Boolean =
    savingThrowPassed(spellSaveDc(caster), attribute, target)

}
