package io.github.tjheslin1.dmspredictor.model

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
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

      val updatedCombatants = if (combatant.creature.creatureType == PlayerCharacter) {

        def abilities[T <: Creature](cmb: Combatant)(
            implicit ca: ClassAbilities[T]): List[(Int, Combatant => Ability[T])] =
          ca.abilities

        val classAbilities: List[(Int, Combatant => Ability[Creature])] = combatant match {
          case Combatant(_, _: Fighter) =>
            abilities(combatant)(implicitly[ClassAbilities[Fighter]])
              .asInstanceOf[List[(Int, Combatant => Ability[Creature])]]
          case _ => List.empty[(Int, Combatant => Ability[Creature])]
        }

        val optAbility = classAbilities.sortBy { case (priority, _) => priority }.find {
          case (_, fighterAbility) =>
            val ability = fighterAbility(combatant)
            ability.conditionMet && ability.triggerMet
        }

        mobToAttack.fold(none[(Combatant, Combatant)]) { mob =>
          optAbility.fold(attackAndDamage(combatant, mob).some) {
            case (_, ability) =>
              val (actedFighter, actedMob) = ability(combatant).useAbility(mob.some)
              val updatedCombatant         = combatant.copy(creature = ability(actedFighter).update)

              actedMob match {
                case Some(updatedMob) => (updatedCombatant, updatedMob).some
                case None             => (updatedCombatant, mob).some
              }
          }
        }
      } else {
        pcToAttack.fold(none[(Combatant, Combatant)])(attackAndDamage(combatant, _).some)
      }

      updatedCombatants.fold(others.append(combatant)) {
        case (attacker, target) =>
          val updatedOthers = others.map(c => if (c.index == target.index) target else c)
          updatedOthers.append(attacker)
      }
    } else
      others.append(combatant)
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
