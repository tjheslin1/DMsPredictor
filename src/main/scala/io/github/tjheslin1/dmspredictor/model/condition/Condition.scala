package io.github.tjheslin1.dmspredictor.model.condition

import cats.Eq
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._

sealed trait ConditionType extends Product with Serializable

case object CharmedCondition   extends ConditionType
case object ParalysedCondition extends ConditionType
case object PoisonedCondition  extends ConditionType
case object TurnedCondition    extends ConditionType

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

  def onConditionApplied[_: RS](creature: Creature): Creature
  def onConditionRemoved[_: RS](creature: Creature): Creature
}

abstract class PassiveCondition extends Condition {

  val saveDc            = 0
  val isHandledOnDamage = false

  def handleStartOfTurn[_: RS](creature: Creature): Creature           = creature
  def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature = creature
  def handleEndOfTurn[_: RS](creature: Creature): Creature             = creature

  def onConditionApplied[_: RS](creature: Creature): Creature = creature
  def onConditionRemoved[_: RS](creature: Creature): Creature = creature
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

  def addCondition[_: RS](combatant: Combatant, condition: Condition): Combatant = {
    val conditionAppliedCreature = condition.onConditionApplied(combatant.creature)

    val updatedConditions = conditionAppliedCreature.conditions :+ condition
    val updatedCreature =
      Creature.creatureConditionsLens.set(updatedConditions)(conditionAppliedCreature)

    Combatant.creatureLens.set(updatedCreature)(combatant)
  }

  def removeCondition[_: RS](creature: Creature, conditionName: String): Creature = {
    val condition         = creature.conditions.find(_.name == conditionName).get
    val updatedConditions = creature.conditions.except(condition)

    val conditionRemovedCreature =
      Creature.creatureConditionsLens.set(updatedConditions)(creature)

    condition.onConditionRemoved(conditionRemovedCreature)
  }

  implicit val conditionEq: Eq[Condition] = (x: Condition, y: Condition) =>
    x.name == y.name && x.saveDc == y.saveDc
}
