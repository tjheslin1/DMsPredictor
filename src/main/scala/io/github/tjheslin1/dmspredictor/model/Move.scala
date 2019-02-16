package io.github.tjheslin1.dmspredictor.model

import cats.syntax.eq._
import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.strategy._
import io.github.tjheslin1.dmspredictor.util.QueueOps._

import scala.collection.immutable.Queue
import scala.util.{Random => JRandom}

object Move extends LazyLogging {

  def takeMove[_: RS](queue: Queue[Combatant], focus: Focus): Queue[Combatant] = {
    val (unactedCombatant, others) = queue.dequeue
    val (pcs, mobs)                = others.partition(_.creature.creatureType == PlayerCharacter)

    val resetUnactedCombatant =
      Combatant.creatureLens.set(unactedCombatant.creature.resetStartOfTurn())(unactedCombatant)

    val conditionHandledCombatant = handleCondition(resetUnactedCombatant)

    if (conditionHandledCombatant.creature.isConscious) {

      val mobToAttack = nextToFocus(mobs, focus)
      val pcToAttack  = nextToFocus(pcs, focus)

      val (actedCombatant, updatedTarget) = conditionHandledCombatant.creature.creatureType match {
        case PlayerCharacter =>
          val optAbility = availableAbility(conditionHandledCombatant, mobToAttack)
          actionAgainstTarget(conditionHandledCombatant, mobToAttack, optAbility)
        case Monster =>
          val optAbility = availableAbility(conditionHandledCombatant, pcToAttack)
          actionAgainstTarget(conditionHandledCombatant, pcToAttack, optAbility)
      }

      val updatedCombatant =
        (Combatant.playerOptional composeLens Player.playerBonusActionUsedLens)
          .set(false)(actedCombatant)

      updatedTarget.fold(others.append(updatedCombatant)) { target =>
        val updatedOthers = others.map(c => if (c === target) target else c)
        updatedOthers.append(updatedCombatant)
      }
    } else {
      val updatedCombatant =
        (Combatant.playerOptional composeLens Player.playerBonusActionUsedLens)
          .set(false)(conditionHandledCombatant)
      others.append(updatedCombatant)
    }
  }

  def handleCondition(combatant: Combatant): Combatant = combatant.creature.conditions match {
    case List() => combatant
    case _      => ???
  }

  private def availableAbility(attacker: Combatant,
                               target: Option[Combatant]): Option[CombatantAbility] =
    attacker.creature.abilities.sortBy(_(attacker).order).find { combatantAbility =>
      val ability = combatantAbility(attacker)
      ability.conditionMet && ability.triggerMet(target)
    }

  private def actionAgainstTarget[_: RS](
      combatant: Combatant,
      toAttack: Option[Combatant],
      optAbility: Option[CombatantAbility]): (Combatant, Option[Combatant]) =
    toAttack.fold((combatant, none[Combatant])) { target =>
      logger.debug(s"${combatant.creature.name} targets ${target.creature.name}")

      optAbility.fold {
        val (updatedAttacker, updatedTarget) = attackAndDamage(combatant, target)
        (updatedAttacker, updatedTarget.some)
      } { ability =>
        val (actedCombatant, targetOfAbility) = ability(combatant).useAbility(target.some)
        val updatedCombatant                  = combatant.copy(creature = ability(actedCombatant).update)

        (updatedCombatant, targetOfAbility)
      }
    }

  def nextToFocus(combatantsToAttack: Queue[Combatant], focus: Focus): Option[Combatant] = {

    val consciousCombatants = combatantsToAttack.filter(_.creature.isConscious)
    focus match {
      case LowestFirst => consciousCombatants.sortBy(_.creature.health).headOption
      case RandomFocus =>
        if (consciousCombatants.isEmpty) None
        else consciousCombatants(JRandom.nextInt(consciousCombatants.size)).some
    }
  }
}
