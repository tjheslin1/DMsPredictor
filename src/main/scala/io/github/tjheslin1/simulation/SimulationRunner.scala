package io.github.tjheslin1.simulation

object SimulationRunner {

  def run(simulation: Simulation, iterations: Int) =
    (1 to iterations).foldLeft((0, 0))((results, _) => {
      val (losses, wins) = results
      simulation.run.result match {
        case Loss => (losses + 1, wins)
        case Win  => (losses, wins + 1)
      }
    })
}
