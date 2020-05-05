package io.github.tjheslin1.dmspredictor.model.condition

import io.github.tjheslin1.dmspredictor.model._
import monocle.macros.Lenses

@Lenses("_") case class Poisoned(
    override val saveDc: Int,
    turnsLeft: Int = Int.MaxValue,
    name: String = "Poisoned"
) extends PassiveCondition {

  val attribute  = Constitution
  val missesTurn = false

  def decrementTurnsLeft(): Condition = Poisoned(saveDc, turnsLeft - 1, name)

  override def onConditionApplied(creature: Creature): Creature =
    Creature.creatureAttackStatusLens.set(Disadvantage)(creature)
}
