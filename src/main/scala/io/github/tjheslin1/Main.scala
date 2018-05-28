package io.github.tjheslin1

import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.monsters.Goblin
import io.github.tjheslin1.simulation.{BasicSimulation, SimulationRunner}

object Main extends App {

  println("Hello World!")

  val pcs            = List(Fighter())
  val mobs           = List(Goblin())
  val (losses, wins) = SimulationRunner.run(BasicSimulation(pcs, mobs), 1000)

  println(s"$losses Losses and $wins Wins")
}
