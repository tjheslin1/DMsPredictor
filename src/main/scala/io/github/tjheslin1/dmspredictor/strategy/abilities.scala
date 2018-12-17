package io.github.tjheslin1.dmspredictor.strategy

import io.github.tjheslin1.dmspredictor.model.{Creature, Level, RS}

trait ClassAbilities[T] {

  def abilities: List[(Int, T => Ability[T])]
}

abstract class Ability[T <: Creature](player: T) {

  def levelRequirement: Level
  def trigger: Boolean
  def condition: Boolean
  def useAbility[_ : RS]: T
  def update: T
}
