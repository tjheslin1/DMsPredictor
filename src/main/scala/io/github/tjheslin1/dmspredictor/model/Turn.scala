package io.github.tjheslin1.dmspredictor.model

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Move.takeMove
import io.github.tjheslin1.dmspredictor.strategy.Focus

import scala.annotation.tailrec
import scala.collection.immutable.Queue

class Turn(initiatives: Map[Int, Initiative])(implicit rollStrategy: RollStrategy)
    extends LazyLogging {

  val initiativeOrder: Queue[Combatant] =
    Queue[Combatant](
      initiatives.toSeq
        .map {
          case (_, initiative) => initiative
        }
        .sortBy(_.score)
        .reverse
        .map(_.combatant): _*)

  def run(focus: Focus): Queue[Combatant] = {

    @tailrec
    def nextCombatant(queue: Queue[Combatant], combatantMovesLeft: Int): Queue[Combatant] =
      if (combatantMovesLeft <= 0) queue
      else {
        val nextTurnQueue = takeMove(queue, focus)
        nextCombatant(nextTurnQueue, combatantMovesLeft - 1)
      }

    logger.debug("-------------")
    nextCombatant(initiativeOrder, initiatives.size)
  }
}

object Turn {

  def apply[_: RS](initiatives: Map[Int, Initiative]): Turn = new Turn(initiatives)
}
