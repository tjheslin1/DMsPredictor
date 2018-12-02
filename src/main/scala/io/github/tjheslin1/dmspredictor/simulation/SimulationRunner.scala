package io.github.tjheslin1.dmspredictor.simulation

import io.github.tjheslin1.dmspredictor.model.RS

object SimulationRunner {

  def run[_: RS](simulation: Simulation, info: String, iterations: Int) =
    (1 to iterations).foldLeft((0, 0))((results, _) => {
      val (losses, wins) = results
      simulation.run(info).result match {
        case Loss       => (losses + 1, wins)
        case Success    => (losses, wins + 1)
        case Unresolved => (losses, wins)
      }
    })
}
