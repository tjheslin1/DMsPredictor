package io.github.tjheslin1.dmspredictor.model.condition

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import monocle.macros.Lenses

@Lenses("_") case class Grappled(saveDc: Int, name: String = Grappled.name) extends Condition {

  val turnsLeft: Int      = 0
  val missesTurn: Boolean = false

  def handle[_: RS](creature: Creature): Creature = {
    val attribute = if (creature.stats.strength > creature.stats.dexterity) Strength else Dexterity

    if (savingThrowPassed(saveDc, attribute, creature)) {
      val grappled          = creature.conditions.find(_.name == name).get
      val updatedConditions = creature.conditions diff List(grappled)

      Creature.creatureConditionsLens.set(updatedConditions)(creature)
    } else creature
  }
}

object Grappled {
  val name: String = "Grappled"
}
