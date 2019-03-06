package io.github.tjheslin1.dmspredictor.model.spellcasting

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.cleric.BaseCleric
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighter
import io.github.tjheslin1.dmspredictor.model.Modifier.attributeModifier
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._

abstract class Spell {

  val name: String
  val school: SchoolOfMagic
  val castingTime: CastingTime
  val spellTargetStyle: SpellTargetStyle
  val damageType: DamageType
  val spellLevel: SpellLevel

  def spellAttackBonus(creature: Creature): Int = creature match {
    case player: Player => player.proficiencyBonus + attributeModifierForSchool(player)
    case monster        => attributeModifierForSchool(monster)
  }

  def spellSaveDc(creature: Creature): Int = creature match {
    case player: Player => 8 + player.proficiencyBonus + attributeModifierForSchool(player)
    case monster        => 8 + attributeModifierForSchool(monster)
  }

  def damage[_: RS](playerLevel: Level): Int
}

object Spell {

  def apply(spellName: String,
            level: SpellLevel,
            schoolOfMagic: SchoolOfMagic,
            castTime: CastingTime,
            offenseStyle: SpellTargetStyle,
            `type`: DamageType,
            dmg: => Int): Spell = new Spell {

    val name                                              = spellName
    val school: spellcasting.SchoolOfMagic                = schoolOfMagic
    val castingTime: spellcasting.CastingTime             = castTime
    val spellTargetStyle: spellcasting.SpellTargetStyle = offenseStyle
    val damageType: DamageType                            = `type`
    val spellLevel: SpellLevel                            = level

    def damage[_: RS](playerLevel: Level): Int = dmg
  }

  def schoolAttribute(creature: Creature): Attribute = creature match {
    case _: BaseFighter => Intelligence
    case _: BaseCleric  => Wisdom
    case _              => ???
  }

  def attributeModifierForSchool(creature: Creature): Int =
    attributeModifier(creature, schoolAttribute(creature))

  def spellSavingThrowPassed[_: RS](caster: Creature,
                                    spell: Spell,
                                    attribute: Attribute,
                                    target: Creature): Boolean =
    SavingThrow.savingThrowPassed(spell.spellSaveDc(caster), attribute, target)
}
