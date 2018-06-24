package io.github.tjheslin1.simulation

import io.github.tjheslin1.model._

case class BasicSimulation(cs: List[Creature]) extends Simulation {

  val creatures = cs

  def run(info: String)(implicit rollStrategy: RollStrategy): SimulationResult = {

    val initiative = Initiative(creatures).rollInitiative

    val (pcs, mobs) = Turn(initiative).run.partition(_.creatureType == PlayerCharacter)

    if (pcs.exists(_.health > 0)) SimulationResult(Success, info)
    else SimulationResult(Loss, info)
  }

}
