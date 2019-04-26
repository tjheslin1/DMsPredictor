package io.github.tjheslin1.dmspredictor.model.condition

import io.github.tjheslin1.dmspredictor.model.{Creature, RS}
import monocle.macros.Lenses

@Lenses("_") case class AcidArrowCondition() extends EndOfTurnCondition {
  val name: String            = "Acid Arrow (condition)"
  val saveDc: Int             = 0
  val turnsLeft: Int          = 1
  val missesTurn: Boolean     = false
  val handleOnDamage: Boolean = false

  def handleEndOfTurn[_: RS](creature: Creature): Creature =
    // TODO take 2d4 acid damage
    ???
}
