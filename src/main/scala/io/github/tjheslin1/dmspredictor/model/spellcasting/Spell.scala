package io.github.tjheslin1.dmspredictor.model.spellcasting

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.classes.fighter.EldritchKnight
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model.Modifier.attributeModifier
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._

abstract class Spell {

  val name: String
  val school: SchoolOfMagic
  val castingTime: CastingTime
  val spellEffect: SpellEffect
  val spellTargetStyle: SpellTargetStyle
  val spellLevel: SpellLevel
  val concentration: Boolean

  def spellAttackBonus(creature: Creature): Int = creature match {
    case playerSpellcaster: Player with SpellCaster =>
      playerSpellcaster.proficiencyBonus + attributeModifierForSchool(playerSpellcaster)
    case spellcaster: SpellCaster => attributeModifierForSchool(spellcaster)
  }

  def spellSaveDc(creature: Creature): Int = creature match {
    case playerSpellcaster: Player with SpellCaster =>
      8 + playerSpellcaster.proficiencyBonus + attributeModifierForSchool(playerSpellcaster)
    case spellcaster: SpellCaster => 8 + attributeModifierForSchool(spellcaster)
  }

  def effect[_: RS](spellCaster: SpellCaster, targets: List[Combatant]): (SpellCaster, List[Combatant])
}

object Spell {

  def schoolAttribute(spellcaster: SpellCaster): Attribute = spellcaster match {
    case _: EldritchKnight => Intelligence
    case _: Cleric         => Wisdom
    case _: BaseCleric     => Wisdom
  }

  def attributeModifierForSchool(spellcaster: SpellCaster): Int =
    attributeModifier(spellcaster, schoolAttribute(spellcaster))

  def spellSavingThrowPassed[_: RS](caster: Creature,
                                    spell: Spell,
                                    attribute: Attribute,
                                    target: Creature): Boolean =
    SavingThrow.savingThrowPassed(spell.spellSaveDc(caster), attribute, target)


}
