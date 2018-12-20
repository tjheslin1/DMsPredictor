package io.github.tjheslin1.dmspredictor.strategy

import io.github.tjheslin1.dmspredictor.model.{Creature, Level, RS}

trait ClassAbilities[T <: Creature] {

  def abilities: List[(Int, T => Ability[T])]
}

abstract class Ability[T <: Creature](player: T) {

  def levelRequirement: Level
  def triggerMet: Boolean
  def conditionMet: Boolean
  def useAbility[_: RS]: T
  def update: T
}
