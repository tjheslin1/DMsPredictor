package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._
import monocle.macros.Lenses

@Lenses("_") case class Charmed(saveDc: Int, name: String = Charmed.name)
    extends StartOfTurnCondition
    with LazyLogging {

  val turnsLeft: Int          = 0
  val missesTurn: Boolean     = true
  val handleOnDamage: Boolean = true

  def handleStartOfTurn[_: RS](creature: Creature): Creature =
    if (savingThrowPassed(saveDc, Wisdom, creature)) {
      val charmed           = creature.conditions.find(_.name == name).get
      val updatedConditions = creature.conditions.except(charmed)

      logger.debug(s"${creature.name} is no longer $name")

      Creature.creatureConditionsLens.set(updatedConditions)(creature)
    } else {
      logger.debug(s"${creature.name} is still $name")
      creature
    }

  override def handleOnDamage[_: RS](creature: Creature): Creature = handleStartOfTurn(creature)
}

object Charmed {
  val name = "Charmed"
}
