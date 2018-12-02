package io.github.tjheslin1.dmspredictor.simulation

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.InitiativeCalculator.updateInitiative
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Focus

import scala.annotation.tailrec

case class BasicSimulation(creatures: List[Creature], focus: Focus) extends Simulation with LazyLogging {

  def run[_: RS](info: String): SimulationResult = {

    @tailrec
    def determineOutcome(initiative: Map[String, Initiative],
                         players: List[Creature],
                         monsters: List[Creature]): SimulationResult =
      if (players.exists(_.health > 0)) {
        if (monsters.exists(_.health > 0)) {

          val (pcs, mobs) = Turn(initiative).run(focus).toList.partition(_.creatureType == PlayerCharacter)

          val updatedInitiative = updateInitiative(initiative, pcs, mobs)

          pcs.foreach(pc => logger.debug(s"pc: ${pc.name} - hp=${pc.health}"))
          mobs.foreach(mob => logger.debug(s"mob: ${mob.name} - hp=${mob.health}"))

          determineOutcome(updatedInitiative, pcs, mobs)
        } else SimulationResult(Success, info)
      } else SimulationResult(Loss, info)

    val initiative = InitiativeCalculator(creatures).rollInitiative()

    val (playerCharacters, monsters) = initiative.toList.map(_._2.creature).partition(_.creatureType == PlayerCharacter)

    determineOutcome(initiative, playerCharacters, monsters)
  }
}
