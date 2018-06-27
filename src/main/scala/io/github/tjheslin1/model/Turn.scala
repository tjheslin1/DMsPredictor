package io.github.tjheslin1.model

import cats.syntax.option._

import scala.collection.immutable.Queue

class Turn(initiatives: Map[String, Initiative])(implicit rollStrategy: RollStrategy) {

  import Actions._

  val initiativeOrder: Queue[Creature] =
    Queue[Creature](initiatives.toSeq.sortBy(_._2.value).reverse.map(_._2.creature): _*)

  def run: Queue[Creature] = {

    def attackFirstEnemy(attacker: Creature, attackee: Creature): (Creature, Option[Creature]) = {
      val (atckr, atckee) = attackAndDamage(attacker, attackee)
      (atckr, atckee.some)
    }

    def nextCreature(queue: Queue[Creature], creaturesMovesLeft: Int): Queue[Creature] = {

      if (creaturesMovesLeft < 1) queue
      else {

        val (creature, waitingQueue) = queue.dequeue
        val (pcs, mobs)              = waitingQueue.partition(_.creatureType == PlayerCharacter)

        val (attacker, attackee) = if (creature.health > 0) {
          creature.creatureType match {
            case PlayerCharacter => attackFirstEnemy(creature, mobs.head)
            case Monster         => attackFirstEnemy(creature, pcs.head)
          }
        } else (creature, None)

        val creatures = waitingQueue.toList.:+(attacker)

        val nextTurnQueue = attackee match {
          case Some(atckee) => Queue[Creature](creatures.map(c => if (c.name == atckee.name) atckee else c): _*)
          case None         => Queue[Creature](creatures: _*)
        }

        nextCreature(nextTurnQueue, creaturesMovesLeft - 1)
      }
    }

    nextCreature(initiativeOrder, initiatives.size)
  }
}

object Turn {

  def apply(initiatives: Map[String, Initiative])(implicit rollStrategy: RollStrategy): Turn = new Turn(initiatives)
}
