package io.github.tjheslin1.dmspredictor.model

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.Modifier.attributeModifier

object SavingThrow {

  def savingThrowPassed[_: RS](dc: Int, attribute: Attribute, target: Creature): Boolean =
    target match {
      case p: Player =>
        if (p.savingThrowProficiencies.exists(_ == attribute)) {
          (D20.roll() + attributeModifier(p, attribute) + p.proficiencyBonus) >= dc
        } else {
          (D20.roll() + attributeModifier(p, attribute)) >= dc
        }
      case c => (D20.roll() + attributeModifier(c, attribute)) >= dc
    }
}
