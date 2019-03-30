package io.github.tjheslin1.dmspredictor.model.condition

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._
import monocle.macros.Lenses

@Lenses("_") case class Paralyzed(saveDc: Int,
                                  turnsLeft: Int,
                                  attribute: Attribute,
                                  name: String = "Paralyzed")
    extends Condition
    with LazyLogging {
  val missesTurn: Boolean     = true
  val handleOnDamage: Boolean = false

  def handle[_: RS](creature: Creature): Creature =
    if (savingThrowPassed(saveDc, attribute, creature)) {
      val paralyzed         = creature.conditions.find(_.name == name).get
      val updatedConditions = creature.conditions.except(paralyzed)

      logger.debug(s"${creature.name} is no longer $name")

      Creature.creatureConditionsLens.set(updatedConditions)(creature)
    } else {
      logger.debug(s"${creature.name} is still $name")
      creature
    }

  def handleOnDamage[_: RS](creature: Creature): Creature = creature
}
