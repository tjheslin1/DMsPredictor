package io.github.tjheslin1.dmspredictor.model

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.model.ability.Ability
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy._
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

    if (conditionHandledCombatant.creature.isConscious) {

      val mobToAttack = nextToFocus(mobs.toList, focus)
      val pcToAttack  = nextToFocus(pcs.toList, focus)

      val attackTarget = conditionHandledCombatant.creature.creatureType match {
        case PlayerCharacter => mobToAttack
        case _               => pcToAttack
      }

      val (actedCombatant, updatedTarget) = {
        val optAbility = availableAbility(conditionHandledCombatant, others.toList)
        actionAgainstTarget(conditionHandledCombatant,
                            attackTarget,
                            others.toList,
                            optAbility,
                            focus)
      }

      val updatedCombatant =
        (Combatant.playerOptional composeLens Player.playerBonusActionUsedLens)
          .set(false)(actedCombatant)

      updatedTarget.fold(others.append(updatedCombatant)) { target =>
        val updatedOthers = others.map {
          case Combatant(target.index, _) => target
          case c                          => c
        }
        updatedOthers.append(updatedCombatant)
      }
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
                                         focus: Focus): (Combatant, Option[Combatant]) =
    optAbility.fold {
      target.fold((combatant, none[Combatant])) { targetToAttack =>
        val (a, t) = attackAndDamage(combatant, targetToAttack)
        (a, t.some)
      }
    } { ability =>
      val (actedCombatant, targetOfAbility) = ability(combatant).useAbility(others, focus)
      val updatedCombatant =
        Combatant.creatureLens.set(ability(actedCombatant).update)(actedCombatant)

      (updatedCombatant, targetOfAbility)
    }
}
