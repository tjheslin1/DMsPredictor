package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._

object Stunned {
  val name = "Stunned"
}

case class Stunned(
    saveDc: Int,
    savingThrowAttribute: Attribute = Constitution,
    name: String = Stunned.name
) extends EndOfTurnCondition
    with LazyLogging {
  val turnsLeft         = 100
  val missesTurn        = true
  val isHandledOnDamage = false

  // can only be removed by passing the saving throw
  def decrementTurnsLeft(): Condition = this

  def handleEndOfTurn[_: RS](creature: Creature): Creature =
    if (savingThrowPassed(saveDc, savingThrowAttribute, creature)) {
      val stunned           = creature.conditions.find(_.name == name).get
      val updatedConditions = creature.conditions.except(stunned)

      logger.debug(s"${creature.name} is no longer $name")

      val conditionUpdatedCreature =
        Creature.creatureConditionsLens.set(updatedConditions)(creature)

      Creature.creatureDefenseStatusLens.set(Regular)(conditionUpdatedCreature)
    } else {
      logger.debug(s"${creature.name} is still $name")

      Creature.creatureDefenseStatusLens.set(Disadvantage)(creature)
    }
}