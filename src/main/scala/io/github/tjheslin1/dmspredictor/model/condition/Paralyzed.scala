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

  def handleEndOfTurn[_: RS](creature: Creature): Creature = {
    val (passed, updatedCreature) = savingThrowPassed(saveDc, attribute, creature)

    if (passed) {
      val paralyzed         = updatedCreature.conditions.find(_.name == name).get
      val updatedConditions = updatedCreature.conditions.except(paralyzed)

      logger.debug(s"${updatedCreature.name} is no longer $name")

      Creature.creatureConditionsLens.set(updatedConditions)(updatedCreature)
    } else {
      logger.debug(s"${updatedCreature.name} is still $name")

      updatedCreature
    }
  }

  override def onConditionApplied[_: RS](creature: Creature): Creature = ???
}
