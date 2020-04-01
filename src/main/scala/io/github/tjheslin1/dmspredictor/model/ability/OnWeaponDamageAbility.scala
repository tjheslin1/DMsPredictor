package io.github.tjheslin1.dmspredictor.model.ability

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Focus

abstract class OnWeaponDamageAbility(combatant: Combatant) extends Ability(combatant) {

  val abilityAction: AbilityAction = OnWeaponDamage

  def triggerOnSingleTargetMet(target: Combatant): Boolean = triggerMet(List(target))
  def damage[_: RS](): Int

  override def useAbility[_: RS](
      others: List[Combatant],
      focus: Focus
  ): (Combatant, List[Combatant]) =
    (combatant, others)

  override def update: Creature = combatant.creature
}
