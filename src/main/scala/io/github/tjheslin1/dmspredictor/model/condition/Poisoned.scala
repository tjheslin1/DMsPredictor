package io.github.tjheslin1.dmspredictor.model.condition

import io.github.tjheslin1.dmspredictor.model._
import monocle.macros.Lenses

@Lenses("_") case class Poisoned(saveDc: Int, turnsLeft: Int, name: String = "Poisoned")
    extends Condition {

  val attribute: Attribute       = Constitution
  val missesTurn: Boolean        = false
  val isHandledOnDamage: Boolean = false

  def decrementTurnsLeft(): Condition = Poisoned(saveDc, turnsLeft - 1, name)

  def handleStartOfTurn[_: RS](creature: Creature): Creature           = creature
  def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature = creature
  def handleEndOfTurn[_: RS](creature: Creature): Creature             = creature
}
