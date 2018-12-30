package io.github.tjheslin1.dmspredictor.model

trait DetermineCritical[T] {
  def attackIsCritical(t: T, roll: Int): Boolean
}

object DetermineCritical {

  def default[T <: Creature]: DetermineCritical[T] = new DetermineCritical[T] {

    def attackIsCritical(t: T, roll: Int): Boolean = roll == 20
  }
}
