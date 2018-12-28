package io.github.tjheslin1.dmspredictor.strategy

import io.github.tjheslin1.dmspredictor.model._

trait ClassAbilities[T <: Creature] {

  def abilities: List[CreatureAbility[T]]
}

abstract class Ability[T <: Creature](player: Combatant) {

  def levelRequirement: Level
  def triggerMet: Boolean
  def conditionMet: Boolean
  def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant])
  def update: T
}
