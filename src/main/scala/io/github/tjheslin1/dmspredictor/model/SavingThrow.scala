package io.github.tjheslin1.dmspredictor.model
import io.github.tjheslin1.dmspredictor.model.Modifier.attributeModifier

object SavingThrow {

  def savingThrowPassed[_: RS](dc: Int, attribute: Attribute, target: Creature): Boolean =
    if ((D20.roll() + attributeModifier(target, attribute)) >= dc)
      true
    else false
}
