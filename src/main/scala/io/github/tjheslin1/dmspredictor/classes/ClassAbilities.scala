package io.github.tjheslin1.dmspredictor.classes

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamageTimes
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability

object ClassAbilities {

  def nextAbilityToUseInConjunction[_: RS](
      attacker: Combatant,
      currentCreatureAbility: CreatureAbility): Option[(Int, Combatant => Ability)] = {

    val (currentPriority, currentAbility) = currentCreatureAbility

    attacker.creature.abilities.sortBy { case (priority, _) => priority }.find {
      case (abilityPriority, creatureAbility) =>
        val ability = creatureAbility(attacker)
        ability.name != currentAbility(attacker).name && abilityPriority < currentPriority &&
        ability.conditionMet && ability.triggerMet
    }
  }

  def useAttackActionTwice[_: RS](attacker: Combatant, target: Combatant): (Combatant, Option[Combatant]) = {
    val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, attacker, target)
    (updatedAttacker, updatedTarget.some)
  }

  def useAdditionalAbility[_: RS](ability: Combatant => Ability,
                                  attacker: Combatant,
                                  abilityTarget: Combatant): (Combatant, Option[Combatant]) = {
    val (updatedAttacker, updatedTargetOfAbility) = ability(attacker).useAbility(abilityTarget.some)
    val updatedAttackingCreature                  = ability(updatedAttacker).update

    val updatedAttackingCombatant = Combatant.creatureLens.set(updatedAttackingCreature)(attacker)
    (updatedAttackingCombatant, updatedTargetOfAbility)
  }
}
