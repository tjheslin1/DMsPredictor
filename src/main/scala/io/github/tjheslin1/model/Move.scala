package io.github.tjheslin1.model

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.model.Actions.attackAndDamage
import io.github.tjheslin1.strategy._
import io.github.tjheslin1.util.QueueOps._
import scala.util.{Random => JRandom}

import scala.collection.immutable.Queue

object Move extends LazyLogging {

  def takeMove(queue: Queue[Creature], focus: Focus)(implicit rollStrategy: RollStrategy): Queue[Creature] = {
    val (creature, others) = queue.dequeue
    val (pcs, mobs)        = others.partition(_.creatureType == PlayerCharacter)

    if (creature.isConscious) {

      val mobToAttack = nextToFocus(mobs, focus)
      val pcToAttack  = nextToFocus(pcs, focus)

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

  private def nextToFocus(creatures: Queue[Creature], focus: Focus): Option[Creature] = {
    val consciousCreatures = creatures.filter(_.health > 0)
    focus match {
      case LowestFirst => consciousCreatures.sortBy(_.health).headOption
      case Random =>
        if (consciousCreatures.isEmpty) None else consciousCreatures(JRandom.nextInt(consciousCreatures.size)).some
    }
  }
}
