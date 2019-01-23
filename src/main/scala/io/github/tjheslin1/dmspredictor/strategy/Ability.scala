package io.github.tjheslin1.dmspredictor.strategy

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability.AbilityAction

abstract class Ability(combatant: Combatant) {

  val name: String
  val order: Int
  val levelRequirement: Level
  val abilityAction: AbilityAction

  val triggerMet: Boolean
  val conditionMet: Boolean

  def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant])
  def update: Creature
}

object Ability {

  sealed trait AbilityAction extends Product with Serializable

  case object Action extends AbilityAction
  case object SingleAttack extends AbilityAction
}