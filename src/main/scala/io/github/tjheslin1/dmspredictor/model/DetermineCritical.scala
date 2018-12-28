package io.github.tjheslin1.dmspredictor.model

trait DetermineCritical[T] {
  def attackIsCritical(roll: Int): Boolean
}

object DetermineCritical {

  def forCreature[T <: Creature]: DetermineCritical[T] = new DetermineCritical[T] {

    val message = "Creature determineCritical"
    def attackIsCritical(roll: Int): Boolean = roll == 20
  }
}
