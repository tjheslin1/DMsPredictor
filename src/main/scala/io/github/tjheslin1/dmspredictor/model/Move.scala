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
      Combatant.creatureLens.set(unactedCombatant.creature.turnReset())(unactedCombatant)

    if (unactedCombatant.creature.isConscious) {

      val mobToAttack = nextToFocus(mobs, focus)
      val pcToAttack  = nextToFocus(pcs, focus)

      val optAbility: Option[CombatantAbility] =
        resetUnactedCombatant.creature.abilities.sortBy(_(resetUnactedCombatant).order).find {
          combatantAbility =>
            val ability = combatantAbility(resetUnactedCombatant)
            ability.conditionMet && ability.triggerMet
        }

      val (actedCombatant, updatedTarget) = resetUnactedCombatant.creature.creatureType match {
        case PlayerCharacter => actionAgainstTarget(resetUnactedCombatant, mobToAttack, optAbility)
        case Monster         => actionAgainstTarget(resetUnactedCombatant, pcToAttack, optAbility)
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
          .set(false)(resetUnactedCombatant)
      others.append(updatedCombatant)
    }
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

  private def nextToFocus(combatants: Queue[Combatant], focus: Focus): Option[Combatant] = {

    val consciousCombatants = combatants.filter(_.creature.isConscious)
    focus match {
      case LowestFirst => consciousCombatants.sortBy(_.creature.health).headOption
      case RandomFocus =>
        if (consciousCombatants.isEmpty) None
        else consciousCombatants(JRandom.nextInt(consciousCombatants.size)).some
    }
  }
}
