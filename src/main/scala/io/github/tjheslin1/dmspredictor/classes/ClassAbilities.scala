package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamageTimes
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.AbilityAction
import io.github.tjheslin1.dmspredictor.strategy.Focus

object ClassAbilities {

  def nextAbilityToUseInConjunction[_: RS](
      attacker: Combatant,
      others: List[Combatant],
      currentOrder: Int,
      suitableAction: NonEmptyList[AbilityAction]): Option[CombatantAbility] =
    attacker.creature.abilities
      .sortBy(ability => ability(attacker).order)
      .find { ability =>
        val nextAbility = ability(attacker)
        suitableAction.toList.contains(nextAbility.abilityAction) &&
        nextAbility.order > currentOrder &&
        nextAbility.conditionMet && nextAbility.triggerMet(others)
      }

  def useAttackActionTwice[_: RS](attacker: Combatant,
                                  target: Combatant): (Combatant, Option[Combatant]) = {
    val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, attacker, target)
    (updatedAttacker, updatedTarget.some)
  }

  def useAdditionalAbility[_: RS](ability: CombatantAbility,
                                  attacker: Combatant,
                                  others: List[Combatant],
                                    focus: Focus): (Combatant, List[Combatant]) = {
    val (updatedAttacker, updatedTargetOfAbility) = ability(attacker).useAbility(others, focus)
    val updatedAttackingCreature                  = ability(updatedAttacker).update

    val updatedAttackingCombatant = Combatant.creatureLens.set(updatedAttackingCreature)(attacker)
    (updatedAttackingCombatant, updatedTargetOfAbility)
  }
}
