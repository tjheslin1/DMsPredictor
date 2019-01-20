package io.github.tjheslin1.dmspredictor.classes

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamageTimes
import io.github.tjheslin1.dmspredictor.model.{Combatant, RS}
import io.github.tjheslin1.dmspredictor.strategy.Ability

object ClassAbilities {

  def nextAbilityToUseInConjunction[_: RS](attacker: Combatant,
                                           currentAbilityName: String): Option[(Int, Combatant => Ability)] = {
    // Currently Action Surge will choose Two Weapon Fighting over Extra Attack
    attacker.creature.abilities.sortBy { case (priority, _) => priority }.find {
      case (_, creatureAbility) =>
        val ability = creatureAbility(attacker)
        ability.name != currentAbilityName && ability.conditionMet && ability.triggerMet
    }
  }

  def useAttackActionTwice[_: RS](attacker: Combatant, target: Combatant): (Combatant, Option[Combatant]) = {
    val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, attacker, target)
    (updatedAttacker, updatedTarget.some)
  }

  def useAdditionalAbility[_: RS](ability: Combatant => Ability, attacker: Combatant, abilityTarget: Combatant): (Combatant, Option[Combatant]) = {
    val (updatedAttacker, updatedAbilityTarget) = ability(attacker).useAbility(abilityTarget.some)
    val updatedAttackingCreature                = ability(updatedAttacker).update

    (attacker.copy(creature = updatedAttackingCreature), updatedAbilityTarget)
  }
}
