package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition._
import monocle.macros.Lenses

object Charmed {
  val name = "Charmed"
}

@Lenses("_") case class Charmed(saveDc: Int) extends StartOfTurnCondition with LazyLogging {

  val name              = Charmed.name
  val turnsLeft         = 100
  val missesTurn        = true
  val isHandledOnDamage = true

  // can only be removed by passing the saving throw
  def decrementTurnsLeft(): Condition = this

  def handleStartOfTurn[_: RS](creature: Creature): Creature = {
    val (passed, updatedCreature) = savingThrowPassed(saveDc, Wisdom, creature)

    if (passed) {
      logger.debug(s"${updatedCreature.name} is no longer $name")

      val charmRemovedCreature = removeCondition(updatedCreature, name)
      val updatedConditions    = charmRemovedCreature.conditions :+ CharmImmunity

      Creature.creatureConditionsLens.set(updatedConditions)(charmRemovedCreature)
    } else {
      logger.debug(s"${updatedCreature.name} is still $name")

      updatedCreature
    }
  }

  override def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature = handleStartOfTurn(
    creature)

  def onConditionApplied[_: RS](creature: Creature): Creature = creature
  def onConditionRemoved[_: RS](creature: Creature): Creature = creature
}
