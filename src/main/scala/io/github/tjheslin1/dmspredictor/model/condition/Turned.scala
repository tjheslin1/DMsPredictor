package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition.removeCondition
import monocle.macros.Lenses

object Turned {
  val name = "Turned"
}

@Lenses("_") case class Turned(saveDc: Int, turnsLeft: Int)
    extends OnDamageCondition
    with LazyLogging {

  val name                       = Turned.name
  val attribute: Attribute       = Wisdom
  val missesTurn: Boolean        = true
  val isHandledOnDamage: Boolean = true

  def decrementTurnsLeft(): Condition = Turned(saveDc, turnsLeft - 1)

  override def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature =
    if (damage > 0) {
      logger.debug(s"${creature.name} is no longer Turned")

      removeCondition(creature, name)
    } else
      creature

  def onConditionApplied(creature: Creature): Creature = creature
  def onConditionRemoved(creature: Creature): Creature = creature
}
