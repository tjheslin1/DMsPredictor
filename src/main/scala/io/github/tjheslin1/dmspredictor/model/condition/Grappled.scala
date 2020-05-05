package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import monocle.macros.Lenses

object Grappled {
  val name = "Grappled"
}

@Lenses("_") case class Grappled(saveDc: Int) extends StartOfTurnCondition with LazyLogging {

  val name                       = Grappled.name
  val turnsLeft: Int             = 100
  val missesTurn: Boolean        = false
  val isHandledOnDamage: Boolean = false

  // can only be removed by passing the saving throw
  def decrementTurnsLeft(): Condition = this

  def handleStartOfTurn[_: RS](creature: Creature): Creature = {
    val attribute = if (creature.stats.strength > creature.stats.dexterity) Strength else Dexterity

    val (passed, updatedCreature) = savingThrowPassed(saveDc, attribute, creature)

    if (passed) {
      logger.debug(s"${updatedCreature.name} is no longer $name")

      Condition.removeCondition(updatedCreature, name)
    } else {
      logger.debug(s"${creature.name} is still $name")

      updatedCreature
    }
  }

  def onConditionApplied(creature: Creature): Creature = creature
}
