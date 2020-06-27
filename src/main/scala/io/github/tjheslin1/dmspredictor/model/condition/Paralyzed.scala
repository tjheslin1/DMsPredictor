package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition.removeCondition
import monocle.macros.Lenses

object Paralyzed {
  val name = "Paralyzed"
}

@Lenses("_") case class Paralyzed(
    saveDc: Int,
    turnsLeft: Int,
    attribute: Attribute
) extends EndOfTurnCondition
    with LazyLogging {

  val name              = Paralyzed.name
  val missesTurn        = true
  val isHandledOnDamage = false

  def decrementTurnsLeft(): Condition = Paralyzed(saveDc, turnsLeft - 1, attribute)

  def handleEndOfTurn[_: RS](creature: Creature): Creature = {
    val (passed, updatedCreature) = savingThrowPassed(saveDc, attribute, creature)

    if (passed) {
      logger.debug(s"${updatedCreature.name} is no longer $name")

      removeCondition(updatedCreature, name)
    } else {
      logger.debug(s"${updatedCreature.name} is still $name")

      onConditionApplied(updatedCreature)
    }
  }

  def onConditionApplied[_: RS](creature: Creature): Creature =
    Creature.creatureDefenseStatusLens.set(Disadvantage)(creature)

  def onConditionRemoved[_: RS](creature: Creature): Creature =
    Creature.creatureDefenseStatusLens.set(Regular)(creature)
}
