package io.github.tjheslin1.dmspredictor.model

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy._
import io.github.tjheslin1.dmspredictor.util.ListOps._
import io.github.tjheslin1.dmspredictor.util.QueueOps._

import scala.collection.immutable.Queue

object Move extends LazyLogging {

  def takeMove[_: RS](queue: Queue[Combatant], focus: Focus): Queue[Combatant] = {
    val (unactedCombatant, others) = queue.dequeue
    val (pcs, mobs)                = others.partition(_.creature.creatureType == PlayerCharacter)

    val resetUnactedCombatant =
      Combatant.creatureLens.set(unactedCombatant.creature.resetStartOfTurn())(unactedCombatant)

    val conditionHandledCombatant =
      handleCondition(resetUnactedCombatant)

    val otherCombatants = others.toList

    if (conditionHandledCombatant.creature.isConscious) {

      val mobToAttack = nextToFocus(mobs.toList, focus)
      val pcToAttack  = nextToFocus(pcs.toList, focus)

      val attackTarget = conditionHandledCombatant.creature.creatureType match {
        case PlayerCharacter => mobToAttack
        case _               => pcToAttack
      }

      val (actedCombatant, updatedTargets) = {
        val optAbility = availableAbility(conditionHandledCombatant, otherCombatants)
        actionAgainstTarget(conditionHandledCombatant,
                            attackTarget,
                            otherCombatants,
                            optAbility,
                            focus)
      }

      val updatedCombatant =
        (Combatant.playerOptional composeLens Player.playerBonusActionUsedLens)
          .set(false)(actedCombatant)

      val updatedOthersTargets = otherCombatants.replace(updatedTargets)
      Queue(updatedOthersTargets:_*).append(updatedCombatant)
    } else {
      val updatedCombatant =
        (Combatant.playerOptional composeLens Player.playerBonusActionUsedLens)
          .set(false)(conditionHandledCombatant)
      others.append(updatedCombatant)
    }
  }

  def handleCondition[_: RS](combatant: Combatant): Combatant =
    Combatant.creatureLens.set {
      combatant.creature.conditions.foldLeft(combatant.creature) {
        case (creature, condition) => condition.handle(creature)
      }
    }(combatant)

  private def availableAbility(attacker: Combatant,
                               others: List[Combatant]): Option[CombatantAbility] =
    attacker.creature.abilities.sortBy(_(attacker).order).find { combatantAbility =>
      val ability = combatantAbility(attacker)
      ability.conditionMet && ability.triggerMet(others)
    }

  private def actionAgainstTarget[_: RS](combatant: Combatant,
                                         target: Option[Combatant],
                                         others: List[Combatant],
                                         optAbility: Option[CombatantAbility],
                                         focus: Focus): (Combatant, List[Combatant]) =
    optAbility.fold {
      target.fold((combatant, List.empty[Combatant])) { targetToAttack =>
        val (updatedAttacker, updatedTarget) = attackAndDamage(combatant, targetToAttack)
        (updatedAttacker, List(updatedTarget))
      }
    } { ability =>
      val (actedCombatant, targetsOfAbility) = ability(combatant).useAbility(others, focus)
      val updatedCombatant =
        Combatant.creatureLens.set(ability(actedCombatant).update)(actedCombatant)

      (updatedCombatant, targetsOfAbility)
    }
}
