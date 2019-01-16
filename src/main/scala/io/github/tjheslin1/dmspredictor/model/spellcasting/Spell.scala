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

  def spellAttack(creature: Creature): Int = creature.proficiencyBonus + attributeModifier(creature)

  def spellSaveDc(creature: Creature): Int = 8 + creature.proficiencyBonus + attributeModifier(creature)

  def damage(implicit rollStrategy: RollStrategy): Int
}

object Spell {

  def schoolAttribute(creature: Creature): Attribute = creature match {
    case _: EldritchKnight => Intelligence
    case _                 => ???
  }

  def attributeModifier(creature: Creature): Int = schoolAttribute(creature) match {
    case Strength     => mod(creature.stats.strength)
    case Dexterity    => mod(creature.stats.dexterity)
    case Constitution => mod(creature.stats.constitution)
    case Wisdom       => mod(creature.stats.wisdom)
    case Intelligence => mod(creature.stats.intelligence)
    case Charisma     => mod(creature.stats.charisma)
  }
}
