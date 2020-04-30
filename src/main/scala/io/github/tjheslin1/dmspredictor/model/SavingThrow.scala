package io.github.tjheslin1.dmspredictor.model

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.Modifier.attributeModifier
import io.github.tjheslin1.dmspredictor.model.condition.Stunned
import io.github.tjheslin1.dmspredictor.monsters.{Legendary, Monster}

object SavingThrow {

  def savingThrowPassed[_: RS](
      dc: Int,
      attribute: Attribute,
      target: Creature
  ): (Boolean, Creature) = {
    val (passed, updatedCreature) = (attribute, target) match {
      case (Strength | Dexterity, creature)
          if creature.conditions.map(_.name).contains(Stunned.name) =>
        (false, creature)
      case (attr, p: Player) =>
        if (p.savingThrowProficiencies.exists(_ == attr)) {
          val passed = (D20.roll() + attributeModifier(p, attr) + p.proficiencyBonus) >= dc

          (passed, p)
        } else {
          val passed = (D20.roll() + attributeModifier(p, attr)) >= dc

          (passed, p)
        }
      case (attr, m: Monster) =>
        val passed = D20.roll() + m.savingThrowScores(attr) >= dc

        (passed, m)
    }

    if (passed) (true, updatedCreature)
    else
      updatedCreature match {
        case l: Legendary if l.legendaryResistances > 0 =>
          val updatedLegendaryResistances = l.legendaryResistances - 1

          (true, Legendary.legendaryResistancesLens.set(updatedLegendaryResistances)(l))
        case _ => (false, updatedCreature)
      }
  }

  def savingThrowWithAdvantagePassed[_: RS](
      dc: Int,
      attribute: Attribute,
      target: Creature
  ): (Boolean, Creature) = {
    val (passed, updatedCreature) = savingThrowPassed(dc, attribute, target)

    if (passed) (true, updatedCreature)
    else savingThrowPassed(dc, attribute, target)
  }

  def savingThrowWithDisadvantagePassed[_: RS](
      dc: Int,
      attribute: Attribute,
      target: Creature
  ): (Boolean, Creature) = {
    val (passed, updatedCreature) = savingThrowPassed(dc, attribute, target)

    if (passed == false) (false, updatedCreature)
    else savingThrowPassed(dc, attribute, target)
  }
}
