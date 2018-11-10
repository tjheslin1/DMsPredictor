package io.github.tjheslin1.dmspredictor.simulation

import io.github.tjheslin1.dmspredictor.model.{Creature, RollStrategy}

trait Simulation {

  def creatures: List[Creature]
  def run(info: String)(implicit rollStrategy: RollStrategy): SimulationResult
}

sealed trait SimulationStatus

case object Success    extends SimulationStatus
case object Loss       extends SimulationStatus
case object Unresolved extends SimulationStatus

case class SimulationResult(result: SimulationStatus, info: String)
