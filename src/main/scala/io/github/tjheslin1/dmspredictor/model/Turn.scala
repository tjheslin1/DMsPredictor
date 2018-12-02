package io.github.tjheslin1.dmspredictor.model

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Move.takeMove
import io.github.tjheslin1.dmspredictor.strategy.Focus

import scala.annotation.tailrec
import scala.collection.immutable.Queue

class Turn(initiatives: Map[String, Initiative])(implicit rollStrategy: RollStrategy) extends LazyLogging {

  val initiativeOrder: Queue[Creature] =
    Queue[Creature](
      initiatives.toSeq
        .map {
          case (_, initiative) => initiative
        }
        .sortBy(_.score)
        .reverse
        .map(_.creature): _*)

  def run(focus: Focus): Queue[Creature] = {

    @tailrec
    def nextCreature(queue: Queue[Creature], creaturesMovesLeft: Int): Queue[Creature] = {

      if (creaturesMovesLeft <= 0) queue
      else {
        val nextTurnQueue = takeMove(queue, focus)
        nextCreature(nextTurnQueue, creaturesMovesLeft - 1)
      }
    }

    nextCreature(initiativeOrder, initiatives.size)
  }
}

object Turn {

  def apply[_: RS](initiatives: Map[String, Initiative]): Turn = new Turn(initiatives)
}
