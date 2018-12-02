package io.github.tjheslin1.dmspredictor.model

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Modifier.mod

import scala.collection.mutable

case class Combatant(index: Int, creature: Creature)

case class Initiative(combatant: Combatant, score: Int)

class InitiativeCalculator(creatures: List[Creature]) extends LazyLogging {

  def rollInitiative[_: RS](): Map[Int, Initiative] =
    creatures.zipWithIndex.map {
      case (c, idx) => idx -> Initiative(Combatant(idx, c), D20.roll() + mod(c.stats.dexterity))
    }.toMap
}

object InitiativeCalculator {

  def apply(creatures: List[Creature]): InitiativeCalculator = new InitiativeCalculator(creatures)

  def updateInitiative(initiative: Map[Int, Initiative],
                       pcs: List[Combatant],
                       mobs: List[Combatant]): Map[Int, Initiative] = {
    val updatedInitiative = mutable.Map[Int, Initiative]()
    pcs.foreach(pc => updatedInitiative.put(pc.index, Initiative(pc, initiative(pc.index).score)))
    mobs.foreach(mob => updatedInitiative.put(mob.index, Initiative(mob, initiative(mob.index).score)))
    updatedInitiative.toMap
  }
}
