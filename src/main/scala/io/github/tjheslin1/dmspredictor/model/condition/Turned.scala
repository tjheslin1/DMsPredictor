package io.github.tjheslin1.dmspredictor.model.condition

import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import monocle.macros.Lenses

@Lenses("_") case class Turned(saveDc: Int, turnsLeft: Int, name: String = "Turned")
    extends Condition {
  val attribute: Attribute = Wisdom

  def handle[_: RS](creature: Creature): Creature =
    if (savingThrowPassed(saveDc, attribute, creature)) {
      val turned            = creature.conditions.find(_.name == name).get
      val updatedConditions = creature.conditions diff List(turned)

      Creature.creatureConditionsLens.set(updatedConditions)(creature)
    }
    else {
      val turned            = creature.conditions.find(_.name == name).get
      val decrementedTurned = Condition.conditionTurnsLeftLens.set(turned.turnsLeft - 1)(turned)

      val updatedCondition = creature.conditions.map {
        case t: Turned => decrementedTurned
        case c         => c
      }

      Creature.creatureConditionsLens.set(updatedCondition)(creature)
    }
}
