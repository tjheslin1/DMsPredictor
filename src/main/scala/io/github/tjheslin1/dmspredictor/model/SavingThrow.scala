package io.github.tjheslin1.dmspredictor.model

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.Modifier.attributeModifier
import io.github.tjheslin1.dmspredictor.monsters.Monster

object SavingThrow {

  def savingThrowPassed[_: RS](dc: Int, attribute: Attribute, target: Creature): Boolean =
    target match {
      case p: Player =>
        if (p.savingThrowProficiencies.exists(_ == attribute)) {
          (D20.roll() + attributeModifier(p, attribute) + p.proficiencyBonus) >= dc
        } else {
          (D20.roll() + attributeModifier(p, attribute)) >= dc
        }
      case m: Monster => (D20.roll() + m.savingThrowScores(attribute)) >= dc
    }
}
