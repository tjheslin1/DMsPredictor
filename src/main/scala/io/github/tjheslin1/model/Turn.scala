package io.github.tjheslin1.model

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.model.Move.takeMove

import scala.annotation.tailrec
import scala.collection.immutable.Queue

class Turn(initiatives: Map[String, Initiative])(implicit rollStrategy: RollStrategy) extends LazyLogging{

  val initiativeOrder: Queue[Creature] =
    Queue[Creature](
      initiatives.toSeq
        .map {
          case (_, initiative) => initiative
        }
        .sortBy(_.score)
        .reverse
        .map(_.creature): _*)

  def run: Queue[Creature] = {

    @tailrec
    def nextCreature(queue: Queue[Creature], creaturesMovesLeft: Int): Queue[Creature] = {

      if (creaturesMovesLeft < 1) queue
      else {
        val nextTurnQueue = takeMove(queue)
        nextCreature(nextTurnQueue, creaturesMovesLeft - 1)
      }
    }

    nextCreature(initiativeOrder, initiatives.size)
  }
}

object Turn {

  def apply(initiatives: Map[String, Initiative])(implicit rollStrategy: RollStrategy): Turn = new Turn(initiatives)
}
