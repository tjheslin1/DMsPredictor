package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.AbilityAction
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.util.ListOps._

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
                                  enemies: List[Combatant],
                                  focus: Focus): (Combatant, List[Combatant]) =
    nextToFocus(enemies, focus).fold((attacker, List.empty[Combatant])) { target =>
      val (updatedAttacker, updatedTarget) = attackAndDamage(attacker, target)

      val updatedEnemies = enemies.replace(updatedTarget)

      nextToFocus(updatedEnemies, focus).fold((updatedAttacker, updatedEnemies)) { nextTarget =>
        val (updatedAttacker2, updatedEnemy2) = attackAndDamage(updatedAttacker, nextTarget)

        val updatedEnemies2 = updatedEnemies.replace(updatedEnemy2)

        (updatedAttacker2, updatedEnemies2)
      }
    }

  def useAdditionalAbility[_: RS](ability: CombatantAbility,
                                  attacker: Combatant,
                                  others: List[Combatant],
                                  focus: Focus): (Combatant, List[Combatant]) = {
    val (updatedAttacker, updatedTargetOfAbility) = ability(attacker).useAbility(others, focus)
    val updatedAttackingCreature                  = ability(updatedAttacker).update

    val updatedAttackingCombatant = Combatant.creatureLens.set(updatedAttackingCreature)(attacker)
    (updatedAttackingCombatant, others.replace(updatedTargetOfAbility))
  }
}
