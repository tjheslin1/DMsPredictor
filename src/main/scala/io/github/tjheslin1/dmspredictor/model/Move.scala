package io.github.tjheslin1.dmspredictor.model

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.strategy._
import io.github.tjheslin1.dmspredictor.util.QueueOps._

import scala.collection.immutable.Queue
import scala.util.{Random => JRandom}

object Move extends LazyLogging {

  def takeMove[_: RS](queue: Queue[Combatant], focus: Focus): Queue[Combatant] = {
    val (combatant, others) = queue.dequeue
    val (pcs, mobs)         = others.partition(_.creature.creatureType == PlayerCharacter)

    if (combatant.creature.isConscious) {

      val mobToAttack = nextToFocus(mobs, focus)
      val pcToAttack  = nextToFocus(pcs, focus)

      val updatedCreatures = combatant.creature.creatureType match {
        case PlayerCharacter => mobToAttack.fold(none[(Combatant, Combatant)])(attackAndDamage(combatant, _).some)
        case Monster         => pcToAttack.fold(none[(Combatant, Combatant)])(attackAndDamage(combatant, _).some)
      }

      updatedCreatures.fold(others.append(combatant)) {
        case (attacker, attackee) =>
          val updatedOthers = others.map(c => if (c.index == attackee.index) attackee else c)
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
