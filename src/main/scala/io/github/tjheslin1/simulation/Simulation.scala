package io.github.tjheslin1.simulation

import io.github.tjheslin1.model.{Creature, PlayerCharacter, RollStrategy}

trait Simulation {

  def characters: List[PlayerCharacter]
  def monsters: List[Creature]
  def run(implicit rollStrategy: RollStrategy): SimulationResult
}

sealed trait SimulationStatus

case object Success extends SimulationStatus
case object Loss    extends SimulationStatus
case object Unresolved extends SimulationStatus

case class SimulationResult(result: SimulationStatus, info: String)
