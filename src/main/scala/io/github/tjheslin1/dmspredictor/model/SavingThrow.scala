package io.github.tjheslin1.dmspredictor.model

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.Modifier.attributeModifier
import io.github.tjheslin1.dmspredictor.model.condition.Stunned
import io.github.tjheslin1.dmspredictor.monsters.Monster

object SavingThrow {

  def savingThrowPassed[_: RS](dc: Int, attribute: Attribute, target: Creature): Boolean =
    (attribute, target) match {
      case (Strength | Dexterity, creature)
          if creature.conditions.map(_.name).contains(Stunned.name) =>
        false
      case (attr, p: Player) =>
        if (p.savingThrowProficiencies.exists(_ == attr)) {
          (D20.roll() + attributeModifier(p, attr) + p.proficiencyBonus) >= dc
        } else {
          (D20.roll() + attributeModifier(p, attr)) >= dc
        }
      case (attr, m: Monster) =>
        D20.roll() + m.savingThrowScores(attr) >= dc
    }

  def savingThrowWithAdvantagePassed[_: RS](
      dc: Int,
      attribute: Attribute,
      target: Creature
  ): Boolean =
    if (savingThrowPassed(dc, attribute, target)) true
    else savingThrowPassed(dc, attribute, target)

  def savingThrowWithDisadvantagePassed[_: RS](
      dc: Int,
      attribute: Attribute,
      target: Creature
  ): Boolean =
    if (savingThrowPassed(dc, attribute, target)) savingThrowPassed(dc, attribute, target)
    else false
}
