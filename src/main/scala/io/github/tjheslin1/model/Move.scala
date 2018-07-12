package io.github.tjheslin1.model

import io.github.tjheslin1.model.Actions.attackAndDamage
import io.github.tjheslin1.util.QueueOps._

import scala.collection.immutable.Queue

object Move {

  def takeMove(queue: Queue[Creature])(implicit rollStrategy: RollStrategy): Queue[Creature] = {
    val (creature, others) = queue.dequeue
    val (pcs, mobs) = others.partition(_.creatureType == PlayerCharacter)

    if (creature.health > 0) {
      val (attacker, attackee) = creature.creatureType match {
        case PlayerCharacter => attackAndDamage(creature, mobs.head)
        case Monster         => attackAndDamage(creature, pcs.head)
      }

      val updatedOthers = others.map(c => if (c.name == attackee.name) attackee else c)
      updatedOthers.append(attacker)
    } else
      others.append(creature)
  }
}
