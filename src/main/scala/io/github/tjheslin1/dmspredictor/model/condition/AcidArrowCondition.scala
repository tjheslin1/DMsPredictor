package io.github.tjheslin1.dmspredictor.model.condition

import io.github.tjheslin1.dmspredictor.model._
import monocle.macros.Lenses
import io.github.tjheslin1.dmspredictor.util.IntOps._

@Lenses("_") case class AcidArrowCondition() extends EndOfTurnCondition {
  val name: String            = "Acid Arrow (condition)"
  val saveDc: Int             = 0
  val turnsLeft: Int          = 1
  val missesTurn: Boolean     = false
  val handleOnDamage: Boolean = false

  def handleEndOfTurn[_: RS](creature: Creature): Creature =
    creature.updateHealth(2 * D4, Acid, Hit)
}
