package io.github.tjheslin1.dmspredictor.model

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.strategy._
import io.github.tjheslin1.dmspredictor.util.QueueOps._

import scala.collection.immutable.Queue
import scala.util.{Random => JRandom}

object Move {

  def takeMove[_: RS](queue: Queue[Combatant], focus: Focus): Queue[Combatant] = {
    val (combatant, others) = queue.dequeue
    val (pcs, mobs)         = others.partition(_.creature.creatureType == PlayerCharacter)

    if (combatant.creature.isConscious) {

      val mobToAttack = nextToFocus(mobs, focus)
      val pcToAttack  = nextToFocus(pcs, focus)

      val optAbility: Option[CreatureAbility[Creature]] =
        combatant.creature.abilities.sortBy { case (priority, _) => priority }.find {
          case (_, creatureAbility) =>
            val ability = creatureAbility(combatant)
            ability.conditionMet && ability.triggerMet
        }

      val updatedCombatants = combatant.creature.creatureType match {
        case PlayerCharacter => actionAgainstTarget(combatant, mobToAttack, optAbility)
        case EnemyMonster    => actionAgainstTarget(combatant, pcToAttack, optAbility)
      }

      updatedCombatants.fold(others.append(combatant)) {
        case (attacker, target) =>
          val updatedOthers = others.map(c => if (c.index == target.index) target else c)
          updatedOthers.append(attacker)
      }
    } else
      others.append(combatant)
  }

  private def actionAgainstTarget[_: RS](combatant: Combatant,
                                         toAttack: Option[Combatant],
                                         optAbility: Option[CreatureAbility[Creature]]) = {
    toAttack.fold(none[(Combatant, Combatant)]) { target =>
      optAbility.fold(attackAndDamage(combatant, target).some) {
        case (_, ability) =>
          val (actedCombatant, actedTarget) = ability(combatant).useAbility(target.some)
          val updatedCombatant              = combatant.copy(creature = ability(actedCombatant).update)

          actedTarget match {
            case Some(updatedTarget) => (updatedCombatant, updatedTarget).some
            case None                => (updatedCombatant, target).some
          }
      }
    }
  }

  private def nextToFocus(combatants: Queue[Combatant], focus: Focus): Option[Combatant] = {

    val consciousCombatants = combatants.filter(_.creature.isConscious)
    focus match {
      case LowestFirst => consciousCombatants.sortBy(_.creature.health).headOption
      case Random =>
        if (consciousCombatants.isEmpty) None else consciousCombatants(JRandom.nextInt(consciousCombatants.size)).some
    }
  }
}
