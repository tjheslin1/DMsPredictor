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
      val conditionUpdatedCreature = removeCondition(updatedCreature, name)

      logger.debug(s"${updatedCreature.name} is no longer $name")

      Creature.creatureDefenseStatusLens.set(Regular)(conditionUpdatedCreature)
    } else {
      logger.debug(s"${updatedCreature.name} is still $name")

      Creature.creatureDefenseStatusLens.set(Disadvantage)(updatedCreature)
    }
  }

  override def onConditionApplied(creature: Creature): Creature =
    Creature.creatureDefenseStatusLens.set(Disadvantage)(creature)
}
