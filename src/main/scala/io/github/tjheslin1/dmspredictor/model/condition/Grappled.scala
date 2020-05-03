package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._
import monocle.macros.Lenses

@Lenses("_") case class Grappled(saveDc: Int, name: String = Grappled.name)
    extends StartOfTurnCondition
    with LazyLogging {

  val turnsLeft: Int             = 100
  val missesTurn: Boolean        = false
  val isHandledOnDamage: Boolean = false

  // can only be removed by passing the saving throw
  def decrementTurnsLeft(): Condition = this

  def handleStartOfTurn[_: RS](creature: Creature): Creature = {
    val attribute = if (creature.stats.strength > creature.stats.dexterity) Strength else Dexterity

    val (passed, updatedCreature) = savingThrowPassed(saveDc, attribute, creature)

    if (passed) {
      val grappled          = updatedCreature.conditions.find(_.name == name).get
      val updatedConditions = updatedCreature.conditions.except(grappled)

      logger.debug(s"${updatedCreature.name} is no longer $name")

      Creature.creatureConditionsLens.set(updatedConditions)(updatedCreature)
    } else {
      logger.debug(s"${creature.name} is still $name")

      updatedCreature
    }
  }

  def onConditionApplied(creature: Creature): Creature = creature
}

object Grappled {
  val name: String = "Grappled"
}
