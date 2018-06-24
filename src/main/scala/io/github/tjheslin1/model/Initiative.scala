package io.github.tjheslin1.model

import io.github.tjheslin1.model.Modifier.mod

class Initiative(creatures: List[Creature]) {

  def rollInitiative(implicit rollStrategy: RollStrategy): Map[Creature, Int] =
    creatures.map(c => (c, D20.roll() + mod(c.stats.dexterity))).toMap
}

object Initiative {

  def apply(creatures: List[Creature]): Initiative = new Initiative(creatures)
}
