package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import monocle.macros.Lenses

@Lenses("_") case class Charmed(saveDc: Int, name: String = Charmed.name)
    extends Condition
    with LazyLogging {

  val turnsLeft: Int          = 0
  val missesTurn: Boolean     = true
  val handleOnDamage: Boolean = true

  def handle[_: RS](creature: Creature): Creature =
    if (savingThrowPassed(saveDc, Wisdom, creature)) {
      val charmed           = creature.conditions.find(_.name == name).get
      val updatedConditions = creature.conditions diff List(charmed)

      logger.debug(s"${creature.name} is no longer Charmed")

      Creature.creatureConditionsLens.set(updatedConditions)(creature)
    } else {
      logger.debug(s"${creature.name} is still Charmed")
      creature
    }

  def handleOnDamage[_: RS](creature: Creature): Creature = handle(creature)
}

object Charmed {
  val name = "Charmed"
}
