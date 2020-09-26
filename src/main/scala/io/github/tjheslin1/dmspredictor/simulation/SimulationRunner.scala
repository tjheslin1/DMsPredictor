package io.github.tjheslin1.dmspredictor.simulation

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.RS

object SimulationRunner extends LazyLogging {

  def run[_: RS](simulation: Simulation, info: String, iterations: Int) =
    (1 to iterations).foldLeft((0, 0)) { (results, _) =>
      val (losses, wins) = results
      logger.debug("\n\n------------ New Simulation ------------\n")
      simulation.run(info).result match {
        case Loss =>
          (losses + 1, wins)
        case Success =>
          (losses, wins + 1)
        case Unresolved =>
          (losses, wins)
      }
    }
}
