package io.github.tjheslin1.model

import cats.syntax.option._

import scala.collection.immutable.Queue

class Turn(initiatives: Map[Creature, Int])(implicit rollStrategy: RollStrategy) {

  import Actions._

  val initiativeOrder: Queue[Creature] = Queue[Creature](initiatives.toSeq.sortBy(_._2).reverse.map(_._1): _*)

  def run: Queue[Creature] = {

    def nextCreature(queue: Queue[Creature], creaturesMovesLeft: Int): Queue[Creature] = {

      if (creaturesMovesLeft < 1) queue
      else {

        val (creature, waitingQueue) = queue.dequeue
        val (pcs, mobs)              = waitingQueue.partition(_.creatureType == PlayerCharacter)

        val (attacker, attackee) = if (creature.health > 0) {
          if (creature.creatureType == PlayerCharacter) {

            val mob = mobs.head

            val (atckr, atckee) = attackAndDamage(creature, mob)
            (atckr, atckee.some)
          } else {

            // this attacks the enemy at the front of the currently rotating queue
            val pc = pcs.head

            val (atckr, atckee) = attackAndDamage(creature, pc)
            (atckr, atckee.some)
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

  def apply(initiatives: Map[Creature, Int])(implicit rollStrategy: RollStrategy): Turn = new Turn(initiatives)
}
