package io.github.tjheslin1.dmspredictor.model.condition

import io.github.tjheslin1.dmspredictor.model._
import monocle.Lens

trait Condition {
  val name: String
  val saveDc: Int
  val turnsLeft: Int

  def handle[_: RS](creature: Creature): Creature
}

object Condition {

  val conditionTurnsLeftLens: Lens[Condition, Int] = Lens[Condition, Int](_.turnsLeft) {
    updatedTurnsLeft =>
      {
        case c: Turned   => Turned._turnsLeft.set(updatedTurnsLeft)(c)
        case c: Poisoned => Poisoned._turnsLeft.set(updatedTurnsLeft)(c)

        case _ => throw new NotImplementedError("Missing a case in conditionTurnsLeftLens")
      }
  }
}
