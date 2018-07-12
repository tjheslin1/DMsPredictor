package io.github.tjheslin1

import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.model.Creature
import io.github.tjheslin1.monsters.Goblin
import io.github.tjheslin1.simulation.{BasicSimulation, SimulationRunner}
import scalax.chart.api._

object Main extends App with scalax.chart.module.Charting {

  import io.github.tjheslin1.model.Dice._

  val creatures: List[Creature] =
    List(Fighter.levelOneFighter().creature, Goblin.levelOneGoblin().creature, Goblin.levelOneGoblin().creature)

  val (losses, wins) = SimulationRunner.run(BasicSimulation(creatures), "Fighter vs Goblin", 1)

  println(s"$wins Wins and $losses Losses")

  val data  = Seq("wins" -> wins, "losses" -> losses)
  val chart = BarChart(data)
  chart.show()
}
