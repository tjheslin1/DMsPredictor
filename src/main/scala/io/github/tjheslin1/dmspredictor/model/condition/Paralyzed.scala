package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._
import monocle.macros.Lenses

@Lenses("_") case class Paralyzed(
    saveDc: Int,
    turnsLeft: Int,
    attribute: Attribute,
    name: String = "Paralyzed"
) extends EndOfTurnCondition
    with LazyLogging {

  val missesTurn        = true
  val isHandledOnDamage = false

  def decrementTurnsLeft(): Condition = Paralyzed(saveDc, turnsLeft - 1, attribute, name)

  def handleEndOfTurn[_: RS](creature: Creature): Creature =
    if (savingThrowPassed(saveDc, attribute, creature)) {
      val paralyzed         = creature.conditions.find(_.name == name).get
      val updatedConditions = creature.conditions.except(paralyzed)

      logger.debug(s"${creature.name} is no longer $name")

      Creature.creatureConditionsLens.set(updatedConditions)(creature)
    } else {
      logger.debug(s"${creature.name} is still $name")
      creature
    }
}
