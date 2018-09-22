package io.github.tjheslin1.simulation

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.model._

import scala.annotation.tailrec
import scala.collection.mutable

case class BasicSimulation(creatures: List[Creature]) extends Simulation with LazyLogging {

  def run(info: String)(implicit rollStrategy: RollStrategy): SimulationResult = {

    def updateInitiative(initiative: Map[String, Initiative], pcs: List[Creature], mobs: List[Creature]): Map[String, Initiative] = {
      val updatedInitiative = mutable.Map[String, Initiative]()
      pcs.foreach(pc => updatedInitiative.put(pc.name, initiative(pc.name)))
      mobs.foreach(mob => updatedInitiative.put(mob.name, initiative(mob.name)))
      updatedInitiative.toMap
    }

    @tailrec
    def determineOutcome(initiative: Map[String, Initiative],
                         players: List[Creature],
                         monsters: List[Creature]): SimulationResult =
      if (players.exists(_.health > 0)) {
        if (monsters.exists(_.health > 0)) {

          val (pcs, mobs) = Turn(initiative).run.toList.partition(_.creatureType == PlayerCharacter)

          val updatedInitiative = updateInitiative(initiative, pcs, mobs)

          pcs.foreach(pc => logger.debug(s"pc: ${pc.name} - hp=${pc.health}"))
          mobs.foreach(mob => logger.debug(s"mob: ${mob.name} - hp=${mob.health}"))

          determineOutcome(updatedInitiative, pcs, mobs)
        } else SimulationResult(Success, info)
      } else SimulationResult(Loss, info)

    val initiative = InitiativeCalculator(creatures).rollInitiative

    val (playerCharacters, monsters) = initiative.toList.map(_._2.creature).partition(_.creatureType == PlayerCharacter)

    determineOutcome(initiative, playerCharacters, monsters)
  }
}
