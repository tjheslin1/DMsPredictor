package io.github.tjheslin1.model

import io.github.tjheslin1.model.Modifier.mod

case class Initiative(creature: Creature, value: Int)

class InitiativeCalculator(creatures: List[Creature]) {

  def rollInitiative(implicit rollStrategy: RollStrategy): Map[String, Initiative] =
    creatures.map(c => c.name -> Initiative(c, D20.roll() + mod(c.stats.dexterity))).toMap
}

object InitiativeCalculator {

  def apply(creatures: List[Creature]): InitiativeCalculator = new InitiativeCalculator(creatures)
}
