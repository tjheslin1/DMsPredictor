package io.github.tjheslin1.dmspredictor.model.ability

import io.github.tjheslin1.dmspredictor.model._

abstract class Ability(combatant: Combatant) {

  val name: String
  val order: Int
  val levelRequirement: Level
  val abilityAction: AbilityAction

  def triggerMet(target: Option[Combatant]): Boolean
  def conditionMet: Boolean

  def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant])
  def update: Creature
}
