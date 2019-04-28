package io.github.tjheslin1.dmspredictor.model.condition

import io.github.tjheslin1.dmspredictor.model._
import monocle.macros.Lenses

@Lenses("_") case class Poisoned(saveDc: Int, turnsLeft: Int, name: String = "Poisoned")
    extends Condition {

  val attribute: Attribute    = Constitution
  val missesTurn: Boolean     = false
  val handleOnDamage: Boolean = false

  def decrementTurnsLeft(): Condition = this.copy(turnsLeft = this.turnsLeft - 1)

  def handleStartOfTurn[_: RS](creature: Creature): Creature = ???
  def handleOnDamage[_: RS](creature: Creature): Creature    = creature
  def handleEndOfTurn[_: RS](creature: Creature): Creature   = ???
}
