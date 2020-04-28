package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._

object Stunned {
  val name = "Stunned"
}

case class Stunned(
    saveDc: Int,
    savingThrowAttribute: Attribute = Constitution,
    name: String = Stunned.name
) extends EndOfTurnCondition
    with LazyLogging {
  val turnsLeft         = 100
  val missesTurn        = true
  val isHandledOnDamage = false

  // can only be removed by passing the saving throw
  def decrementTurnsLeft(): Condition = this

  def handleEndOfTurn[_: RS](creature: Creature): Creature = ???
  //    if (savingThrowPassed(saveDc, savingThrowAttribute, creature))
}
