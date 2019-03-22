package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.AbilityAction
import io.github.tjheslin1.dmspredictor.strategy.{Focus, Target}
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters
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
                                  others: List[Combatant],
                                  focus: Focus): (Combatant, List[Combatant]) = {
    nextToFocus(monsters(others), focus).fold((attacker, List.empty[Combatant])) { target =>

    val (updatedAttacker, updatedTarget, updatedOthers) =
      attackAndDamage(attacker, target, others.except(target))

      val allUpdatedOthers = updatedOthers.replace(updatedTarget)

    nextToFocus(monsters(allUpdatedOthers), focus).fold((updatedAttacker, allUpdatedOthers)) { nextTarget =>
      val (updatedAttacker2, updatedEnemy2, updatedOtherEnemies2) =
        attackAndDamage(updatedAttacker, nextTarget, allUpdatedOthers)

      (updatedAttacker2, updatedOtherEnemies2.replace(updatedEnemy2))
    }
  }
                                  }

  def useAdditionalAbility[_: RS](ability: CombatantAbility,
                                  attacker: Combatant,
                                  others: List[Combatant],
                                  focus: Focus): (Combatant, List[Combatant]) = {
    val (updatedAttacker, updatedOthers) = ability(attacker).useAbility(others, focus)
    val updatedAttackingCreature                  = ability(updatedAttacker).update

    val updatedAttackingCombatant = Combatant.creatureLens.set(updatedAttackingCreature)(attacker)

    (updatedAttackingCombatant, updatedOthers)
  }
}
