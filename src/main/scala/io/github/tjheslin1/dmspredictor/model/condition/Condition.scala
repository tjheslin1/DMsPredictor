package io.github.tjheslin1.dmspredictor.model.condition

import cats.Eq
import io.github.tjheslin1.dmspredictor.model._
import monocle.Lens

trait Condition {
  val name: String
  val saveDc: Int
  val turnsLeft: Int
  val missesTurn: Boolean
  val isHandledOnDamage: Boolean

  def decrementTurnsLeft(): Condition

  def handleStartOfTurn[_: RS](creature: Creature): Creature
  def handleEndOfTurn[_: RS](creature: Creature): Creature
  def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature
}

abstract class PassiveCondition extends Condition {

  def handleStartOfTurn[_: RS](creature: Creature): Creature           = creature
  def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature = creature
  def handleEndOfTurn[_: RS](creature: Creature): Creature             = creature
}

abstract class StartOfTurnCondition extends Condition {

  def handleEndOfTurn[_: RS](creature: Creature): Creature             = creature
  def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature = creature
}

abstract class EndOfTurnCondition extends Condition {

  def handleStartOfTurn[_: RS](creature: Creature): Creature           = creature
  def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature = creature
}

abstract class OnDamageCondition extends Condition {

  def handleStartOfTurn[_: RS](creature: Creature): Creature = creature
  def handleEndOfTurn[_: RS](creature: Creature): Creature   = creature
}

object Condition {

  implicit val conditionEq: Eq[Condition] = (x: Condition, y: Condition) =>
    x.name == y.name && x.saveDc == y.saveDc

  val conditionTurnsLeftLens: Lens[Condition, Int] = Lens[Condition, Int](_.turnsLeft) {
    updatedTurnsLeft =>
      {
        case c: Turned   => Turned._turnsLeft.set(updatedTurnsLeft)(c)
        case c: Poisoned => Poisoned._turnsLeft.set(updatedTurnsLeft)(c)

        case _ => throw new NotImplementedError("Missing a case in conditionTurnsLeftLens")
      }
  }
}
