package io.github.tjheslin1.model

class Initiative(creatures: List[Creature]) { //(implicit rollStrategy: RollStrategy) {

  import Initiative._

  def rollInitiative: List[(Creature, Int)] = creatures.map(c => (c, D20.roll() + c.stats.dexterity.score)).sorted(initiativeOrdering)
}

object Initiative {

  val initiativeOrdering: Ordering[(Creature, Int)] = (x: (Creature, Int), y: (Creature, Int)) =>
    if (x._2 < y._2) -1 else if (x._2 > x._2) 1 else 0

  def apply(creatures: List[Creature]): Initiative = new Initiative(creatures)
}
