package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._
import monocle.macros.Lenses

object Turned

@Lenses("_") case class Turned(saveDc: Int, turnsLeft: Int, name: String = "Turned")
    extends OnDamageCondition
    with LazyLogging {

  val attribute: Attribute       = Wisdom
  val missesTurn: Boolean        = true
  val isHandledOnDamage: Boolean = true

  def decrementTurnsLeft(): Condition = Turned(saveDc, turnsLeft - 1, name)

  override def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature = {
    val turned            = creature.conditions.find(_.name == name).get
    val updatedConditions = creature.conditions.except(turned)

    if (damage > 0) {
      logger.debug(s"${creature.name} is no longer Turned")
      Creature.creatureConditionsLens.set(updatedConditions)(creature)
    } else {
      creature
    }
  }

  override def onConditionApplied(creature: Creature): Creature = ???
}
