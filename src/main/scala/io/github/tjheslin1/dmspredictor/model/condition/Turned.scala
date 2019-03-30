package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._
import monocle.macros.Lenses

@Lenses("_") case class Turned(saveDc: Int, turnsLeft: Int, name: String = "Turned")
    extends Condition
    with LazyLogging {

  val attribute: Attribute    = Wisdom
  val missesTurn: Boolean     = true
  val handleOnDamage: Boolean = true

  def handle[_: RS](creature: Creature): Creature = {
    val turned            = creature.conditions.find(_.name == name).get
    val decrementedTurned = Condition.conditionTurnsLeftLens.set(turned.turnsLeft - 1)(turned)

    val updatedCondition = creature.conditions.map {
      case _: Turned => decrementedTurned
      case c         => c
    }

    logger.debug(s"${creature.name} is still Turned (${decrementedTurned.turnsLeft} turns left)")

    Creature.creatureConditionsLens.set(updatedCondition)(creature)
  }

  def handleOnDamage[_: RS](creature: Creature): Creature = {
    val turned            = creature.conditions.find(_.name == name).get
    val updatedConditions = creature.conditions.except(turned)

    logger.debug(s"${creature.name} is no longer Turned")

    Creature.creatureConditionsLens.set(updatedConditions)(creature)
  }
}
