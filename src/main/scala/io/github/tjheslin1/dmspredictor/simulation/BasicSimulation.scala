package io.github.tjheslin1.dmspredictor.simulation

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.InitiativeCalculator.updateInitiative
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Focus

import scala.annotation.tailrec

case class BasicSimulation(creatures: List[Creature], focus: Focus)
    extends Simulation
    with LazyLogging {

  def run[_: RS](info: String): SimulationResult = {

    @tailrec
    def determineOutcome(initiative: Map[Int, Initiative],
                         players: List[Combatant],
                         monsters: List[Combatant]): SimulationResult =
      if (players.exists(_.creature.isConscious)) {
        if (monsters.exists(_.creature.isConscious)) {

          val (pcs, mobs) =
            Turn(initiative).run(focus).toList.partition(_.creature.creatureType == PlayerCharacter)

          val updatedInitiative = updateInitiative(initiative, pcs, mobs)

          pcs.foreach(pc => logger.debug(s"pc: ${pc.creature.name} - hp=${pc.creature.health}"))
          mobs.foreach(mob =>
            logger.debug(s"mob: ${mob.creature.name} - hp=${mob.creature.health}"))

          determineOutcome(updatedInitiative, pcs, mobs)
        } else SimulationResult(Success, info)
      } else SimulationResult(Loss, info)

    val initiative = InitiativeCalculator(creatures).rollInitiative()

    val (playerCharacters, monsters) =
      initiative.toList.map(_._2.combatant).partition(_.creature.creatureType == PlayerCharacter)

    determineOutcome(initiative, playerCharacters, monsters)
  }
}
