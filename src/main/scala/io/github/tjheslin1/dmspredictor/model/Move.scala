package io.github.tjheslin1.dmspredictor.model

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.model.ability.{AbilityAction, BonusAction}
import io.github.tjheslin1.dmspredictor.model.condition.{EndOfTurnCondition, StartOfTurnCondition}
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy._
import io.github.tjheslin1.dmspredictor.util.ListOps._
import io.github.tjheslin1.dmspredictor.util.QueueOps._

import scala.collection.immutable.Queue

object Move extends LazyLogging {

  def takeMove[_: RS](queue: Queue[Combatant], focus: Focus): Queue[Combatant] = {
    val (unactedCombatant, others) = queue.dequeue
    val (pcs, mobs)                = others.partition(_.creature.creatureType == PlayerCharacter)

    val resetUnactedCombatant = {
      val resetCombatant =
        Combatant.creatureLens.set(unactedCombatant.creature.resetStartOfTurn())(unactedCombatant)

      val bonusActionUnusedCombatant =
        (Combatant.playerOptional composeLens Player.playerBonusActionUsedLens)
          .set(false)(resetCombatant)

      (Combatant.playerOptional composeLens Player.playerReactionUsedLens)
        .set(false)(bonusActionUnusedCombatant)
    }

    val (conditionHandledCombatant, missesTurn) =
      handleStartOfTurnConditions(decrementConditionsTurnsLeft(resetUnactedCombatant))

    val otherCombatants = others.toList

    if (conditionHandledCombatant.creature.isConscious && missesTurn == false) {

      val mobToAttack = nextToFocus(conditionHandledCombatant, mobs.toList, focus)
      val pcToAttack  = nextToFocus(conditionHandledCombatant, pcs.toList, focus)

      val attackTarget = conditionHandledCombatant.creature.creatureType match {
        case PlayerCharacter => mobToAttack
        case _               => pcToAttack
      }

      val (actedCombatant, updatedTargets) = {
        val optAbility = availableActionAbility(conditionHandledCombatant, otherCombatants)
        actionAgainstTarget(conditionHandledCombatant,
                            attackTarget,
                            otherCombatants,
                            optAbility,
                            focus)
      }

      val updatedOthersTargets = otherCombatants.replace(updatedTargets)

      val (bonusActionUsedCombatant, updatedOthers) =
        availableBonusAction(actedCombatant, updatedOthersTargets).fold(
          (actedCombatant, updatedOthersTargets)) { bonusActionAbility =>
          useBonusActionAbility(actedCombatant, updatedOthersTargets, bonusActionAbility, focus)
        }

      val endOfTurnConditionHandledCombatant =
        handleEndOfTurnConditions(bonusActionUsedCombatant)

      Queue(updatedOthers: _*).append(endOfTurnConditionHandledCombatant)
    } else {
      others.append(conditionHandledCombatant)
    }
  }

  def decrementConditionsTurnsLeft(combatant: Combatant): Combatant =
    (Combatant.creatureLens composeLens Creature.creatureConditionsLens).set {
      combatant.creature.conditions.map(_.decrementTurnsLeft()).filter { condition =>
        if (condition.turnsLeft > 0) true
        else {
          logger.debug(s"${condition.name} has run out on ${combatant.creature.name}")
          false
        }
    }
    }(combatant)

  def handleStartOfTurnConditions[_: RS](combatant: Combatant): (Combatant, Boolean) = {
    val updatedCombatant = Combatant.creatureLens.set {
      combatant.creature.conditions.foldLeft(combatant.creature) {
        case (creature, condition: StartOfTurnCondition) => condition.handleStartOfTurn(creature)
        case (creature, _)                               => creature
      }
    }(combatant)

    val missesTurn = updatedCombatant.creature.conditions.exists(_.missesTurn)

    (updatedCombatant, missesTurn)
  }

  def handleEndOfTurnConditions[_: RS](combatant: Combatant): Combatant =
    Combatant.creatureLens.set {
      combatant.creature.conditions.foldLeft(combatant.creature) {
        case (creature, condition: EndOfTurnCondition) => condition.handleEndOfTurn(creature)
        case (creature, _)                             => creature
      }
    }(combatant)

  def useBonusActionAbility[_: RS](combatant: Combatant,
                                   others: List[Combatant],
                                   bonusActionAbility: CombatantAbility,
                                   focus: Focus): (Combatant, List[Combatant]) = {
    val (bonusActionUsedAttacker, updatedOthersAfterBonusAction) =
      bonusActionAbility(combatant).useAbility(others, focus)

    val updatedBonusActionUsedAttacker = Combatant.creatureLens.set(
      bonusActionAbility(bonusActionUsedAttacker).update)(bonusActionUsedAttacker)

    (updatedBonusActionUsedAttacker, others.replace(updatedOthersAfterBonusAction))
  }

  private def availableActionAbility(attacker: Combatant,
                                     others: List[Combatant]): Option[CombatantAbility] =
    attacker.creature.abilities.sortBy(_(attacker).order).find { combatantAbility =>
      val ability = combatantAbility(attacker)
      AbilityAction.MainAction.toList
        .contains(ability.abilityAction) && ability.conditionMet && ability
        .triggerMet(others)
    }

  private def availableBonusAction(attacker: Combatant,
                                   others: List[Combatant]): Option[CombatantAbility] =
    attacker.creature match {
      case player: Player if player.bonusActionUsed == false =>
        attacker.creature.abilities.sortBy(_(attacker).order).find { combatantAbility =>
          val ability = combatantAbility(attacker)
          ability.abilityAction == BonusAction && ability.conditionMet && ability.triggerMet(others)
        }
      case _ => None
    }

  private def actionAgainstTarget[_: RS](combatant: Combatant,
                                         target: Option[Combatant],
                                         others: List[Combatant],
                                         optAbility: Option[CombatantAbility],
                                         focus: Focus): (Combatant, List[Combatant]) =
    optAbility.fold {
      target.fold((combatant, others)) { targetToAttack =>
        val (updatedAttacker, updatedTarget, updatedOthers) =
          attackAndDamage(combatant, targetToAttack, others)

        (updatedAttacker, updatedOthers.replace(updatedTarget))
      }
    } { ability =>
      val (actedCombatant, targetsOfAbility) = ability(combatant).useAbility(others, focus)
      val updatedCombatant =
        Combatant.creatureLens.set(ability(actedCombatant).update)(actedCombatant)

      (updatedCombatant, others.replace(targetsOfAbility))
    }
}
