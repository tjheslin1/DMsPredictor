package io.github.tjheslin1.dmspredictor.model.ability

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Focus

abstract class Ability(combatant: Combatant) {

  val name: String
  val order: Int
  val levelRequirement: Level
  val abilityAction: AbilityAction

  def triggerMet(others: List[Combatant]): Boolean
  def conditionMet: Boolean

  def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, Option[Combatant])
  def update: Creature
}
