package io.github.tjheslin1.dmspredictor

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.simulation.{BasicSimulation, SimulationRunner}
import io.github.tjheslin1.dmspredictor.strategy._

object Main extends App with scalax.chart.module.Charting with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

//  val creatures: List[Creature] =
//    List(Fighter.levelOneFighter(), Goblin.levelOneGoblin(), Goblin.levelOneGoblin())

  val barbarian = Barbarian.levelOneBarbarian().copy(level = LevelFive, name = "Barbarian")

  val creatures: List[Creature] =
    List(barbarian, Goblin.levelOneGoblin(), Goblin.levelOneGoblin())

  creatures.foreach(c => logger.debug(s"${c.name}: ${c.health}/${c.maxHealth}\n$c\n"))

  val simulation = "Fighter vs Goblin"
  println(s"$simulation simulation started")

  val (losses, wins) =
    SimulationRunner.run(BasicSimulation(creatures, LowestFirst), simulation, 1)

  println(s"$wins Wins and $losses Losses")

  val data  = Seq("wins" -> wins, "losses" -> losses)
  val chart = BarChart(data)
  chart.show(title = simulation)
}
