package io.github.tjheslin1.dmspredictor.model.condition

import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import monocle.macros.Lenses

@Lenses("_") case class Charmed(saveDc: Int, name: String = Charmed.name) extends Condition {

  val turnsLeft: Int      = 0
  val missesTurn: Boolean = true

  def handle[_: RS](creature: Creature): Creature =
    if (savingThrowPassed(saveDc, Wisdom, creature)) {
      val charmed           = creature.conditions.find(_.name == name).get
      val updatedConditions = creature.conditions diff List(charmed)

      Creature.creatureConditionsLens.set(updatedConditions)(creature)
    } else creature
}

object Charmed {
  val name = "Charmed"
}
