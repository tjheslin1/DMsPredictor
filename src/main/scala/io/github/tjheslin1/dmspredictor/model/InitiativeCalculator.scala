package io.github.tjheslin1.dmspredictor.model

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Modifier.mod

import scala.collection.mutable

case class Initiative(creature: Creature, score: Int)

class InitiativeCalculator(creatures: List[Creature]) extends LazyLogging {

  def rollInitiative(implicit rollStrategy: RollStrategy): Map[String, Initiative] =
    creatures.map(c => c.name -> Initiative(c, D20.roll() + mod(c.stats.dexterity))).toMap
}

object InitiativeCalculator {

  def apply(creatures: List[Creature]): InitiativeCalculator = new InitiativeCalculator(creatures)

  def updateInitiative(initiative: Map[String, Initiative],
                       pcs: List[Creature],
                       mobs: List[Creature]): Map[String, Initiative] = {
    val updatedInitiative = mutable.Map[String, Initiative]()
    pcs.foreach(pc => updatedInitiative.put(pc.name, Initiative(pc, initiative(pc.name).score)))
    mobs.foreach(mob => updatedInitiative.put(mob.name, Initiative(mob, initiative(mob.name).score)))
    updatedInitiative.toMap
  }
}
