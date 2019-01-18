package io.github.tjheslin1.dmspredictor.model.spellcasting

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.EldritchKnight
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._

abstract class Spell {

  val school: SchoolOfMagic
  val castingTime: CastingTime
  val spellOffenseStyle: SpellOffenseStyle
  val damageType: DamageType
  val spellLevel: SpellLevel

  def spellAttackBonus(creature: Creature): Int = creature.proficiencyBonus + attributeModifierForSchool(creature)

  def spellSaveDc(creature: Creature): Int = 8 + creature.proficiencyBonus + attributeModifierForSchool(creature)

  def damage(implicit rollStrategy: RollStrategy): Int
}

object Spell {

  def schoolAttribute(creature: Creature): Attribute = creature match {
    case _: EldritchKnight => Intelligence
    case _                 => ???
  }

  def attributeModifierForSchool(creature: Creature): Int =
    attributeModifier(creature, schoolAttribute(creature))

  def attributeModifier(creature: Creature, attribute: Attribute): Int = attribute match {
    case Strength     => mod(creature.stats.strength)
    case Dexterity    => mod(creature.stats.dexterity)
    case Constitution => mod(creature.stats.constitution)
    case Wisdom       => mod(creature.stats.wisdom)
    case Intelligence => mod(creature.stats.intelligence)
    case Charisma     => mod(creature.stats.charisma)
  }
}
