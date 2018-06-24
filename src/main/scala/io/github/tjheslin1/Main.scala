package io.github.tjheslin1

import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.monsters.Goblin
import io.github.tjheslin1.simulation.{BasicSimulation, SimulationRunner}
import scalax.chart.api._

object Main extends App with scalax.chart.module.Charting {

  import io.github.tjheslin1.model.Dice._

  val pcs  = List(Fighter())
  val mobs = List(Goblin())

  val (losses, wins) = SimulationRunner.run(BasicSimulation(pcs, mobs), 1000)

  println(s"$losses Losses and $wins Wins")

  val data = Seq("wins" -> wins, "losses" -> losses)
  val chart = BarChart(data)
  chart.show()
}
