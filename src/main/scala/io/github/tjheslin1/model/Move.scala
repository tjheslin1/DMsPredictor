package io.github.tjheslin1.model

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.model.Actions.attackAndDamage
import io.github.tjheslin1.util.QueueOps._

import scala.collection.immutable.Queue

object Move extends LazyLogging {

  def takeMove(queue: Queue[Creature])(implicit rollStrategy: RollStrategy): Queue[Creature] = {
    val (creature, others) = queue.dequeue
    val (pcs, mobs)        = others.partition(_.creatureType == PlayerCharacter)

    if (creature.isConscious) {

      // TODO attack enemy with lowest hp
      val mobToAttack = mobs.find(_.health > 0)
      val pcToAttack  = pcs.find(_.health > 0)

      val updatedCreatures = creature.creatureType match {
        case PlayerCharacter => mobToAttack.fold(none[(Creature, Creature)])(attackAndDamage(creature, _).some)
        case Monster         => pcToAttack.fold(none[(Creature, Creature)])(attackAndDamage(creature, _).some)
      }

      updatedCreatures.fold(others.append(creature)) {
        case (attacker, attackee) =>
          val updatedOthers = others.map(c => if (c.name == attackee.name) attackee else c)
          updatedOthers.append(attacker)
      }
    } else
      others.append(creature)
  }
}
