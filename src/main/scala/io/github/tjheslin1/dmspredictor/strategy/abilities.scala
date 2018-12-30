package io.github.tjheslin1.dmspredictor.strategy

import io.github.tjheslin1.dmspredictor.model._

trait ClassAbilities[T] {

  def abilities: List[CreatureAbility[T]]
}

abstract class Ability[T](combatant: Combatant) {

  def levelRequirement: Level
  def triggerMet: Boolean
  def conditionMet: Boolean
  def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant])
  def update: T
}
