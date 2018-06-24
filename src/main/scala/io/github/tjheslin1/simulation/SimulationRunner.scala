package io.github.tjheslin1.simulation

import io.github.tjheslin1.model.RollStrategy

object SimulationRunner {

  def run(simulation: Simulation, iterations: Int)(implicit rollStrategy: RollStrategy) =
    (1 to iterations).foldLeft((0, 0))((results, _) => {
      val (losses, wins) = results
      simulation.run.result match {
        case Loss    => (losses + 1, wins)
        case Success => (losses, wins + 1)
        case Unresolved => (losses, wins)
      }
    })
}
