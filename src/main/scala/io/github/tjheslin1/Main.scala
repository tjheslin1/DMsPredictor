package io.github.tjheslin1

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.model.{Creature, Dice}
import io.github.tjheslin1.monsters.Goblin
import io.github.tjheslin1.simulation.{BasicSimulation, SimulationRunner}
import io.github.tjheslin1.strategy._
import scalax.chart.api._

object Main extends App with scalax.chart.module.Charting with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

  val creatures: List[Creature] =
    List(Fighter.levelOneFighter().creature, Goblin.levelOneGoblin().creature, Goblin.levelOneGoblin().creature)

  val simulation     = "Fighter vs Goblin"
  val (losses, wins) = SimulationRunner.run(BasicSimulation(creatures, LowestFirst), simulation, 100000)

  logger.debug(s"$simulation simulation started")
  println(s"$wins Wins and $losses Losses")

  val data  = Seq("wins" -> wins, "losses" -> losses)
  val chart = BarChart(data)
  chart.show()
}
