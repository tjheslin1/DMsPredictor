package io.github.tjheslin1.dmspredictor.strategy

import io.github.tjheslin1.dmspredictor.model._

abstract class Ability(combatant: Combatant) {

  val levelRequirement: Level
  def triggerMet: Boolean
  def conditionMet: Boolean
  def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant])
  def update: Creature
}
