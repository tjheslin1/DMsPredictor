package io.github.tjheslin1.dmspredictor.model.ability

import io.github.tjheslin1.dmspredictor.model.Combatant
import io.github.tjheslin1.dmspredictor.strategy.Focus

abstract class OnWeaponDamageAbility(combatant: Combatant) extends Ability(combatant) {

  val abilityAction: AbilityAction = OnWeaponDamage

  def triggerOnSingleTargetMet(target: Combatant) = triggerMet(List(target), ???)
}
