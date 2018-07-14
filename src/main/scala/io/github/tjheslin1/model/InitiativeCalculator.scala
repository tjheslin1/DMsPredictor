package io.github.tjheslin1.model

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.model.Modifier.mod

case class Initiative(creature: Creature, score: Int)

class InitiativeCalculator(creatures: List[Creature]) extends LazyLogging{

  def rollInitiative(implicit rollStrategy: RollStrategy): Map[String, Initiative] =
    creatures.map(c => {
      val initiative = Initiative(c, D20.roll() + mod(c.stats.dexterity))
      logger.info(s"${c.name} initiatve = ${initiative.score}")
      c.name -> initiative
    }).toMap
}

object InitiativeCalculator {

  def apply(creatures: List[Creature]): InitiativeCalculator = new InitiativeCalculator(creatures)
}
