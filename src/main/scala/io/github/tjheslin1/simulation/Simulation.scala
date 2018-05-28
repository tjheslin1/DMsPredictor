package io.github.tjheslin1.simulation

import io.github.tjheslin1.model.{Creature, PlayerCharacter}

trait Simulation {

  def characters: List[PlayerCharacter]
  def monsters: List[Creature]
  def run: SimulationResult
}

sealed trait SimulationStatus

case object Win     extends SimulationStatus
case object Loss    extends SimulationStatus
case object Unknown extends SimulationStatus

case class SimulationResult(result: SimulationStatus, info: String)
