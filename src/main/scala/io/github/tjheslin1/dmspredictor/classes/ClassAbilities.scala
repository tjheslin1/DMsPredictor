package io.github.tjheslin1.dmspredictor.classes

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamageTimes
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability.AbilityAction

object ClassAbilities {

  def nextAbilityToUseInConjunction[_: RS](attacker: Combatant, currentOrder: Int): Option[CombatantAbility] =
    attacker.creature.abilities
      .sortBy(ability => ability(attacker).order)
      .find { ability =>
        val nextAbility = ability(attacker)
        nextAbility.order > currentOrder && nextAbility.conditionMet && nextAbility.triggerMet
      }

  def useAttackActionTwice[_: RS](attacker: Combatant, target: Combatant): (Combatant, Option[Combatant]) = {
    val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, attacker, target)
    (updatedAttacker, updatedTarget.some)
  }

  def useAdditionalAbility[_: RS](ability: CombatantAbility,
                                  attacker: Combatant,
                                  abilityTarget: Combatant): (Combatant, Option[Combatant]) = {
    val (updatedAttacker, updatedTargetOfAbility) = ability(attacker).useAbility(abilityTarget.some)
    val updatedAttackingCreature                  = ability(updatedAttacker).update

    val updatedAttackingCombatant = Combatant.creatureLens.set(updatedAttackingCreature)(attacker)
    (updatedAttackingCombatant, updatedTargetOfAbility)
  }
}
