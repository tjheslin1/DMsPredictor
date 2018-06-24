package io.github.tjheslin1.simulation

import io.github.tjheslin1.model._

case class BasicSimulation(characters: List[PlayerCharacter], monsters: List[Creature]) extends Simulation {

  def pcs: List[PlayerCharacter] = characters

  def mobs: List[Creature] = monsters

  def run(implicit rollStrategy: RollStrategy): SimulationResult = {

    val initiative = Initiative(pcs ++ mobs).rollInitiative

    Turn(initiative).run

    SimulationResult(Success, "todo")
  }

}
