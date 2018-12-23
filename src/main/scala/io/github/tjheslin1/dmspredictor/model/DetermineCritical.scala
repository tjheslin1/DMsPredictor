package io.github.tjheslin1.dmspredictor.model

trait DetermineCritical[T <: Creature] {

  val message: String
  def attackIsCritical(roll: Int): Boolean
}
